const std = @import("std");

const common = @import("common.zig");
const scanner_ = @import("scanner.zig");
const Scanner = scanner_.Scanner;
const TokenType = scanner_.TokenType;
const Token = scanner_.Token;
const chunk_ = @import("chunk.zig");
const Chunk = chunk_.Chunk;
const OpCode = chunk_.OpCode;
const value_ = @import("value.zig");
const Value = value_.Value;
const object_ = @import("object.zig");
const gc_ = @import("gc.zig");
const vm = @import("vm.zig");

const Parser = struct {
    current: scanner_.Token,
    previous: scanner_.Token,
    had_error: bool,
    panic_mode: bool,
};

const Precedence = enum(u8) {
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR, // or
    PREC_AND, // and
    PREC_EQUALITY, // == !=
    PREC_COMPARISON, // < > <= >=
    PREC_TERM, // + -
    PREC_FACTOR, // * /
    PREC_UNARY, // ! -
    PREC_CALL, // . ()
    PREC_PRIMARY,
};

const ParseFunc = *const fn (bool) void;

const ParseRule = struct {
    prefix: ?ParseFunc = null,
    infix: ?ParseFunc = null,
    precedence: Precedence = .PREC_NONE,
};

const Local = struct {
    name: Token,
    depth: i32,
    is_captured: bool,
};

pub const UpValue = struct {
    index: u16,
    is_local: bool,
};

const FunctionType = enum(u2) {
    TYPE_FUNCTION,
    TYPE_INITIALIZER,
    TYPE_METHOD,
    TYPE_SCRIPT,
};

const Compiler = struct {
    enclosing: ?*Compiler = null,
    function: ?*object_.ObjFunction = null,
    function_type: FunctionType = .TYPE_SCRIPT,
    locals: [256]Local = undefined,
    local_count: u16 = 0,
    upvalues: [256]UpValue = undefined,
    scope_depth: i32 = 0,

    fn init(self: *Compiler, typ: FunctionType) void {
        self.enclosing = current;
        self.function_type = typ;

        const func = object_.ObjFunction.allocate();
        self.function = func;

        current = self;
        if (typ != .TYPE_SCRIPT) {
            current.?.function.?.name = object_.ObjString.copy(parser.previous.lexeme);
        }
        var local = &self.locals[self.local_count];
        self.local_count += 1;
        local.depth = 0;
        local.is_captured = false;

        if (typ != .TYPE_FUNCTION) {
            local.name.lexeme = "this";
        } else {
            local.name.lexeme = "";
        }
    }
};

const ClassCompiler = struct {
    enclosing: ?*ClassCompiler,
    has_superclass: bool = false,
};

var parser: Parser = undefined;
var scanner: Scanner = undefined;
var current: ?*Compiler = null;
var current_class: ?*ClassCompiler = null;

inline fn current_chunk() *Chunk {
    return &current.?.function.?.chunk;
}

fn error_at(token: scanner_.Token, msg: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    std.debug.print("[line {}] Error", .{token.line});

    if (token.type == .TOKEN_EOF) {
        std.debug.print(" at end", .{});
    } else if (token.type == .TOKEN_ERROR) {} else std.debug.print(" at '{s}'", .{token.lexeme});
    std.debug.print(": {s}\n", .{msg});
    parser.had_error = true;
}

fn error_(msg: []const u8) void {
    error_at(parser.previous, msg);
}

fn error_at_current(msg: []const u8) void {
    error_at(parser.current, msg);
}

fn advance() void {
    parser.previous = parser.current;
    while (true) {
        parser.current = scanner.scan_token();
        if (parser.current.type != .TOKEN_ERROR) break;

        error_at_current(parser.current.lexeme);
    }
}

fn consume(typ: TokenType, msg: []const u8) void {
    if (parser.current.type == typ) {
        advance();
        return;
    }
    error_at_current(msg);
}

inline fn check(typ: TokenType) bool {
    return parser.current.type == typ;
}

inline fn match(typ: TokenType) bool {
    if (!check(typ)) return false;
    advance();
    return true;
}

inline fn emit_op(code: OpCode) void {
    current_chunk().write_op(code, parser.previous.line);
}

inline fn emit_byte(byte: u8) void {
    current_chunk().write_byte(byte, parser.previous.line);
}

inline fn emit_pair(code: OpCode, byte: u8) void {
    emit_op(code);
    emit_byte(byte);
}

inline fn emitLoop(loop_start: usize) void {
    emit_op(.OP_LOOP);

    const offset = current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        error_("Loop body too large.");
    }

    emit_byte(@truncate(offset >> 8));
    emit_byte(@truncate(offset));
}

inline fn emit_bytes(byte1: u8, byte2: u8) void {
    emit_byte(byte1);
    emit_byte(byte2);
}

inline fn emitJump(op: OpCode) usize {
    emit_op(op);
    emit_byte(0xff);
    emit_byte(0xff);
    return current_chunk().code.items.len - 2;
}

inline fn emit_return() void {
    if (current.?.function_type == .TYPE_INITIALIZER) {
        emit_pair(.OP_GET_LOCAL, 0);
    } else {
        emit_op(.OP_NIL);
    }
    emit_op(.OP_RETURN);
}

fn make_constant(value: Value) u8 {
    const constant = current_chunk().add_const(value);
    if (constant > 255) {
        error_("Too many constants in one chunk.");
        return 0;
    }
    return @intCast(constant);
}

fn emit_constant(value: Value) void {
    emit_op(.OP_CONSTANT);
    emit_byte(make_constant(value));
}

fn patchJump(offset: usize) void {
    const jump = current_chunk().code.items.len - offset - 2;
    if (jump > std.math.maxInt(u16)) {
        error_("Too much code to jump over.");
    }
    current_chunk().code.items[offset] = @truncate(jump >> 8);
    current_chunk().code.items[offset + 1] = @truncate(jump);
}

fn end_compiler() *object_.ObjFunction {
    emit_return();
    const func = current.?.function.?;
    if (comptime common.debug_print_code) {
        if (!parser.had_error)
            current_chunk().disassemble(if (func.name == null) "script" else func.name.?.chars);
    }
    current = current.?.enclosing;
    return func;
}

inline fn beginScope() void {
    current.?.scope_depth += 1;
}

inline fn endScope() void {
    current.?.scope_depth -= 1;
    while (current.?.local_count > 0 and current.?.locals[current.?.local_count - 1].depth > current.?.scope_depth) {
        if (current.?.locals[current.?.local_count - 1].is_captured) {
            emit_op(.OP_CLOSE_UPVALUE);
        } else {
            emit_op(.OP_POP);
        }
        current.?.local_count -= 1;
    }
}

fn resolveLocal(compiler: *Compiler, token: *const Token) ?u8 {
    var i: i32 = @as(i32, compiler.local_count) - 1;
    while (i >= 0) : (i -= 1) {
        const local = &compiler.locals[@intCast(i)];
        if (std.mem.eql(u8, local.name.lexeme, token.lexeme)) {
            if (local.depth == -1) {
                error_("Can't read local variable in its own initializer.");
            }
            return @intCast(i);
        }
    }
    return null;
}

fn addUpValue(compiler: *Compiler, index: u8, is_local: bool) u8 {
    const upvalue_count = compiler.function.?.upvalue_count;
    var i: u16 = 0;
    while (i < upvalue_count) : (i += 1) {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) return @intCast(i);
    }

    if (upvalue_count == 256) {
        error_("Too many closure variables in function.");
        return 0;
    }

    var upvalue = &compiler.upvalues[compiler.function.?.upvalue_count];
    upvalue.index = index;
    upvalue.is_local = is_local;
    defer compiler.function.?.upvalue_count += 1;
    return @intCast(compiler.function.?.upvalue_count);
}

fn resolveUpValue(compiler: *Compiler, token: *const Token) ?u8 {
    if (compiler.enclosing == null) return null;
    const local = resolveLocal(compiler.enclosing.?, token);
    if (local != null) {
        compiler.enclosing.?.locals[@intCast(local.?)].is_captured = true;
        return addUpValue(compiler, local.?, true);
    }

    const upvalue = resolveUpValue(compiler.enclosing.?, token);
    if (upvalue != null) {
        return addUpValue(compiler, upvalue.?, false);
    }
    return null;
}

fn binary(can_assign: bool) void {
    _ = can_assign;
    const token_type = parser.previous.type;
    const rule = get_rule(token_type);
    parse_precedence(@as(Precedence, @enumFromInt(@intFromEnum(rule.precedence) + 1)));

    switch (token_type) {
        .TOKEN_BANG_EQUAL => emit_op(.OP_NOT_EQUAL),
        .TOKEN_EQUAL_EQUAL => emit_op(.OP_EQUAL),
        .TOKEN_GREATER => emit_op(.OP_GREATER),
        .TOKEN_GREATER_EQUAL => emit_op(.OP_GREATER_EQUAL),
        .TOKEN_LESS => emit_op(.OP_LESS),
        .TOKEN_LESS_EQUAL => emit_op(.OP_LESS_EQUAL),
        .TOKEN_PLUS => emit_op(.OP_ADD),
        .TOKEN_MINUS => emit_op(.OP_SUBTRACT),
        .TOKEN_STAR => emit_op(.OP_MULTIPLY),
        .TOKEN_SLASH => emit_op(.OP_DIVIDE),
        else => unreachable,
    }
}

fn call(_: bool) void {
    const arg_count = argumentList();
    emit_op(.OP_CALL);
    emit_byte(arg_count);
}

fn dot(can_assign: bool) void {
    consume(.TOKEN_IDENTIFIER, "Expect property name after '.'.");
    const name = identifierConstant(&parser.previous);

    if (can_assign and match(.TOKEN_EQUAL)) {
        expression();
        emit_pair(.OP_SET_PROPERTY, name);
    } else if (match(.TOKEN_LEFT_PAREN)) {
        const arg_count = argumentList();
        emit_pair(.OP_INVOKE, name);
        emit_byte(arg_count);
    } else {
        emit_pair(.OP_GET_PROPERTY, name);
    }
}

fn literal(_: bool) void {
    switch (parser.previous.type) {
        .TOKEN_NIL => emit_op(.OP_NIL),
        .TOKEN_TRUE => emit_op(.OP_TRUE),
        .TOKEN_FALSE => emit_op(.OP_FALSE),
        else => unreachable,
    }
}

fn grouping(_: bool) void {
    expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

fn number(_: bool) void {
    const value = std.fmt.parseFloat(f64, parser.previous.lexeme) catch unreachable;
    emit_constant(Value.number(value));
}

fn or_(_: bool) void {
    const jump = emitJump(.OP_JUMP_IF_TRUE);

    emit_op(.OP_POP);
    parse_precedence(.PREC_OR);

    patchJump(jump);
}

fn string(_: bool) void {
    const sub_obj = object_.ObjString.copy(parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1]);
    const constant = Value.obj(&sub_obj.obj);
    vm.push(constant);
    emit_constant(constant);
    _ = vm.pop();
}

fn namedVariable(name: Token, can_assign: bool) void {
    var set_op: OpCode = undefined;
    var get_op: OpCode = undefined;

    var arg = resolveLocal(current.?, &name);
    if (arg != null) {
        set_op = .OP_SET_LOCAL;
        get_op = .OP_GET_LOCAL;
    } else {
        arg = resolveUpValue(current.?, &name);
        if (arg != null) {
            set_op = .OP_SET_UPVALUE;
            get_op = .OP_GET_UPVALUE;
        } else {
            arg = identifierConstant(&name);
            set_op = .OP_SET_GLOBAL;
            get_op = .OP_GET_GLOBAL;
        }
    }

    if (can_assign and match(.TOKEN_EQUAL)) {
        expression();
        emit_op(set_op);
        emit_byte(arg.?);
    } else {
        emit_op(get_op);
        emit_byte(arg.?);
    }
}

fn variable(can_assign: bool) void {
    namedVariable(parser.previous, can_assign);
}

fn syntheticToken(comptime text: []const u8) Token {
    return comptime Token{ .type = .TOKEN_NIL, .lexeme = text, .line = 0 };
}

fn super(_: bool) void {
    if (current_class == null) {
        error_("Can't use 'super' outside of a class.");
    } else if (!current_class.?.has_superclass) {
        error_("Can't use 'super' in a class with no superclass.");
    }

    consume(.TOKEN_DOT, "Expect '.' after 'super'.");
    consume(.TOKEN_IDENTIFIER, "Expect superclass method name.");
    const name = identifierConstant(&parser.previous);

    namedVariable(syntheticToken("this"), false);
    if (match(.TOKEN_LEFT_PAREN)) {
        const arg_count = argumentList();
        namedVariable(syntheticToken("super"), false);
        emit_pair(.OP_SUPER_INVOKE, name);
        emit_byte(arg_count);
    } else {
        namedVariable(syntheticToken("super"), false);
        emit_pair(.OP_GET_SUPER, name);
    }
}

fn this(_: bool) void {
    if (current_class == null) {
        error_("Can't use 'this' outside of a class.");
        return;
    }
    variable(false);
}

fn unary(_: bool) void {
    const token_type = parser.previous.type;

    parse_precedence(.PREC_UNARY);

    switch (token_type) {
        .TOKEN_BANG => emit_op(.OP_NOT),
        .TOKEN_MINUS => emit_op(.OP_NEGATE),
        else => unreachable,
    }
}

fn make_rules() [@typeInfo(TokenType).Enum.fields.len]ParseRule {
    var arr_: [@typeInfo(TokenType).Enum.fields.len]ParseRule = undefined;

    arr_[@intFromEnum(TokenType.TOKEN_LEFT_PAREN)] = ParseRule{ .prefix = grouping, .infix = call, .precedence = .PREC_CALL };
    arr_[@intFromEnum(TokenType.TOKEN_RIGHT_PAREN)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_LEFT_BRACE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_RIGHT_BRACE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_COMMA)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_DOT)] = ParseRule{ .infix = dot, .precedence = .PREC_CALL };
    arr_[@intFromEnum(TokenType.TOKEN_MINUS)] = ParseRule{ .prefix = unary, .infix = binary, .precedence = .PREC_TERM };
    arr_[@intFromEnum(TokenType.TOKEN_PLUS)] = ParseRule{ .infix = binary, .precedence = .PREC_TERM };
    arr_[@intFromEnum(TokenType.TOKEN_SEMICOLON)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_SLASH)] = ParseRule{ .infix = binary, .precedence = .PREC_FACTOR };
    arr_[@intFromEnum(TokenType.TOKEN_STAR)] = ParseRule{ .infix = binary, .precedence = .PREC_FACTOR };
    arr_[@intFromEnum(TokenType.TOKEN_BANG)] = ParseRule{ .prefix = unary };
    arr_[@intFromEnum(TokenType.TOKEN_BANG_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_EQUALITY };
    arr_[@intFromEnum(TokenType.TOKEN_EQUAL)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_EQUAL_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_EQUALITY };
    arr_[@intFromEnum(TokenType.TOKEN_GREATER)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@intFromEnum(TokenType.TOKEN_GREATER_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@intFromEnum(TokenType.TOKEN_LESS)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@intFromEnum(TokenType.TOKEN_LESS_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@intFromEnum(TokenType.TOKEN_IDENTIFIER)] = ParseRule{ .prefix = variable };
    arr_[@intFromEnum(TokenType.TOKEN_STRING)] = ParseRule{ .prefix = string };
    arr_[@intFromEnum(TokenType.TOKEN_NUMBER)] = ParseRule{ .prefix = number };
    arr_[@intFromEnum(TokenType.TOKEN_AND)] = ParseRule{ .infix = and_, .precedence = .PREC_AND };
    arr_[@intFromEnum(TokenType.TOKEN_CLASS)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_ELSE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_FALSE)] = ParseRule{ .prefix = literal };
    arr_[@intFromEnum(TokenType.TOKEN_FOR)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_FUN)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_IF)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_NIL)] = ParseRule{ .prefix = literal };
    arr_[@intFromEnum(TokenType.TOKEN_OR)] = ParseRule{ .infix = or_, .precedence = .PREC_OR };
    arr_[@intFromEnum(TokenType.TOKEN_PRINT)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_RETURN)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_SUPER)] = ParseRule{ .prefix = super };
    arr_[@intFromEnum(TokenType.TOKEN_THIS)] = ParseRule{ .prefix = this };
    arr_[@intFromEnum(TokenType.TOKEN_TRUE)] = ParseRule{ .prefix = literal };
    arr_[@intFromEnum(TokenType.TOKEN_VAR)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_WHILE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_ERROR)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_EOF)] = ParseRule{};
    return arr_;
}
const rules = make_rules();

inline fn get_rule(type_: TokenType) *const ParseRule {
    return &rules[@intFromEnum(type_)];
}

fn parse_precedence(precedence: Precedence) void {
    advance();

    const token_type = parser.previous.type;
    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.PREC_ASSIGNMENT);
    if (rules[@intFromEnum(token_type)].prefix) |rule| {
        rule(can_assign);
    } else {
        error_("Expect expression.");
    }

    const precedence_int = @intFromEnum(precedence);
    while (precedence_int <= @intFromEnum(get_rule(parser.current.type).precedence)) {
        advance();
        const infix_rule = get_rule(parser.previous.type).infix.?;
        infix_rule(can_assign);
    }

    if (can_assign and match(.TOKEN_EQUAL)) {
        error_("Invalid assignment target.");
    }
}

fn identifierConstant(name: *const Token) u8 {
    return make_constant(Value.obj(&object_.ObjString.copy(name.lexeme).obj));
}

fn addLocal(name: Token) void {
    if (current.?.local_count == 256) {
        error_("Too many local variables in function.");
        return;
    }
    var local = &current.?.locals[current.?.local_count];
    current.?.local_count += 1;
    local.name = name;
    local.is_captured = false;
    local.depth = -1;
}

fn declareVariable() void {
    if (current.?.scope_depth == 0) return;

    const name = &parser.previous;
    var i = @as(i32, current.?.local_count) - 1;
    while (i >= 0) : (i -= 1) {
        const local = &current.?.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < current.?.scope_depth) {
            break;
        }

        if (std.mem.eql(u8, name.lexeme, local.name.lexeme)) {
            error_("Already a variable with this name in this scope.");
        }
    }
    addLocal(name.*);
}

fn parseVariable(errorMsg: []const u8) u8 {
    consume(.TOKEN_IDENTIFIER, errorMsg);

    declareVariable();
    if (current.?.scope_depth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn markInitialized() void {
    if (current.?.scope_depth == 0) return;
    current.?.locals[current.?.local_count - 1].depth = current.?.scope_depth;
}

fn defineVariable(global: u8) void {
    if (current.?.scope_depth > 0) {
        markInitialized();
        return;
    }
    emit_op(.OP_DEFINE_GLOBAL);
    emit_byte(global);
}

fn argumentList() u8 {
    var arg_count: u8 = 0;
    if (!check(.TOKEN_RIGHT_PAREN)) {
        while (true) {
            expression();
            if (arg_count == 255) {
                error_("Can't have more than 255 arguments.");
            } else {
                arg_count += 1;
            }
            if (!match(.TOKEN_COMMA)) break;
        }
    }
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

fn and_(_: bool) void {
    const jump = emitJump(.OP_JUMP_IF_FALSE);

    emit_op(.OP_POP);
    parse_precedence(.PREC_AND);

    patchJump(jump);
}

fn expression() void {
    parse_precedence(.PREC_ASSIGNMENT);
}

fn block() void {
    while (!check(.TOKEN_RIGHT_BRACE) and !check(.TOKEN_EOF)) {
        declaration();
    }
    consume(.TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

fn function(typ: FunctionType) void {
    var compiler = Compiler{};
    compiler.init(typ);

    beginScope();
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after function name.");
    if (!check(.TOKEN_RIGHT_PAREN)) {
        while (true) {
            current.?.function.?.arity += 1;
            if (current.?.function.?.arity > 255) {
                error_at_current("Can't have more than 255 parameters.");
            }
            const param = parseVariable("Expect parameter name.");
            defineVariable(param);
            if (!match(.TOKEN_COMMA)) break;
        }
    }
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
    consume(.TOKEN_LEFT_BRACE, "Expect '{' before function body.");
    block();

    const func = end_compiler();
    const constant = make_constant(Value.obj(&func.obj)); // reachable by gc
    emit_op(.OP_CLOSURE);
    emit_byte(constant);

    var i: u16 = 0;
    while (i < func.upvalue_count) : (i += 1) {
        emit_byte(if (compiler.upvalues[i].is_local) 1 else 0);
        emit_byte(@intCast(compiler.upvalues[i].index));
    }
}

fn method() void {
    consume(.TOKEN_IDENTIFIER, "Expect method name.");
    const constant = identifierConstant(&parser.previous);

    var function_type: FunctionType = .TYPE_METHOD;
    if (std.mem.eql(u8, "init", parser.previous.lexeme))
        function_type = .TYPE_INITIALIZER;
    function(function_type);
    emit_pair(.OP_METHOD, constant);
}

fn classDeclaration() void {
    consume(.TOKEN_IDENTIFIER, "Expect class name.");
    const class_name = parser.previous;

    const name_constant = identifierConstant(&parser.previous);
    declareVariable();
    emit_op(.OP_CLASS);
    emit_byte(name_constant);
    defineVariable(name_constant);

    var class_compiler = ClassCompiler{ .enclosing = current_class, .has_superclass = false };
    current_class = &class_compiler;

    if (match(.TOKEN_LESS)) {
        consume(.TOKEN_IDENTIFIER, "Expect superclass name.");
        variable(false);

        if (std.mem.eql(u8, class_name.lexeme, parser.previous.lexeme)) {
            error_("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(syntheticToken("super"));
        defineVariable(0);

        namedVariable(class_name, false);
        emit_op(.OP_INHERIT);
        class_compiler.has_superclass = true;
    }

    namedVariable(class_name, false);
    consume(.TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(.TOKEN_RIGHT_BRACE) and !check(.TOKEN_EOF)) {
        method();
    }
    consume(.TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emit_op(.OP_POP);

    if (class_compiler.has_superclass) {
        endScope();
    }

    current_class = class_compiler.enclosing;
}

fn functionDeclaration() void {
    const global = parseVariable("Expect function name.");
    markInitialized();
    function(.TYPE_FUNCTION);
    defineVariable(global);
}

fn varDeclaration() void {
    const global = parseVariable("Expect variable name.");

    if (match(.TOKEN_EQUAL)) {
        expression();
    } else {
        emit_op(.OP_NIL);
    }
    consume(.TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(global);
}

fn expressionStatement() void {
    expression();
    consume(.TOKEN_SEMICOLON, "Expect ';' after expression.");
    emit_op(.OP_POP);
}

fn forStatement() void {
    beginScope();

    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(.TOKEN_SEMICOLON)) {} else if (match(.TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    var loop_start = current_chunk().code.items.len;
    var exit_jump: ?usize = null;
    if (!match(.TOKEN_SEMICOLON)) {
        expression();
        consume(.TOKEN_SEMICOLON, "Expect ';' after loop condition.");
        exit_jump = emitJump(.OP_JUMP_IF_FALSE);
        emit_op(.OP_POP);
    }

    if (!match(.TOKEN_RIGHT_PAREN)) {
        const body_jump = emitJump(.OP_JUMP);
        const incr_start = current_chunk().code.items.len;
        expression();
        emit_op(.OP_POP);
        consume(.TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loop_start);
        loop_start = incr_start;
        patchJump(body_jump);
    }

    statement();
    emitLoop(loop_start);
    if (exit_jump != null) {
        patchJump(exit_jump.?);
        emit_op(.OP_POP);
    }
    endScope();
}

fn ifStatement() void {
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const thenJump = emitJump(.OP_JUMP_IF_FALSE);
    emit_op(.OP_POP);
    statement();

    const elseJump = emitJump(.OP_JUMP);
    patchJump(thenJump);
    emit_op(.OP_POP);

    if (match(.TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

fn printStatement() void {
    expression();
    consume(.TOKEN_SEMICOLON, "Expect ';' after value.");
    emit_op(.OP_PRINT);
}

fn returnStatement() void {
    if (current.?.function_type == .TYPE_SCRIPT) {
        error_("Can't return from top-level code.");
    }

    if (match(.TOKEN_SEMICOLON)) {
        emit_return();
    } else {
        if (current.?.function_type == .TYPE_INITIALIZER) {
            error_("Can't return a value from an initializer.");
        }
        expression();
        consume(.TOKEN_SEMICOLON, "Expect ';' after return value.");
        emit_op(.OP_RETURN);
    }
}

fn whileStatement() void {
    const loop_start = current_chunk().code.items.len;
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    const exit_jump = emitJump(.OP_JUMP_IF_FALSE);
    emit_op(.OP_POP);
    statement();
    emitLoop(loop_start);

    patchJump(exit_jump);
    emit_op(.OP_POP);
}

fn switchStatement() void {
    consume(.TOKEN_LEFT_PAREN, "Expect '(' after switch.");
    expression();
    consume(.TOKEN_RIGHT_PAREN, "Expect ')' after switch expression.");

    consume(.TOKEN_LEFT_BRACE, "Expect '{' after swtich expression.");

    var end_jumps: [256]usize = undefined;
    var n_end_jumps: usize = 0;
    while (match(.TOKEN_CASE)) {
        if (n_end_jumps > 256) {
            error_("Can not have more than 256 case statements in this lox implementation.");
            break;
        }
        expression();
        const next_jump = emitJump(.OP_JUMP_IF_NEQUAL);
        consume(.TOKEN_COLON, "Expect ':' after case.");

        while (!check(.TOKEN_CASE) and !check(.TOKEN_DEFAULT) and !check(.TOKEN_RIGHT_BRACE)) {
            statement();
        }
        end_jumps[n_end_jumps] = emitJump(.OP_JUMP);
        n_end_jumps += 1;

        patchJump(next_jump);
    }

    if (match(.TOKEN_DEFAULT)) {
        consume(.TOKEN_COLON, "Expect ':' after default.");
        while (!check(.TOKEN_RIGHT_BRACE)) {
            statement();
        }
    }

    for (0..n_end_jumps) |i| {
        patchJump(end_jumps[i]);
    }

    emit_op(.OP_POP);
    consume(.TOKEN_RIGHT_BRACE, "Expect '}' to end switch statement.");
}

fn synchronize() void {
    parser.panic_mode = false;
    while (parser.current.type != .TOKEN_EOF) {
        if (parser.previous.type == .TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            .TOKEN_CLASS, .TOKEN_FUN, .TOKEN_VAR, .TOKEN_FOR, .TOKEN_IF, .TOKEN_WHILE, .TOKEN_PRINT, .TOKEN_RETURN => return,
            else => {},
        }
        advance();
    }
}

fn declaration() void {
    if (match(.TOKEN_CLASS)) {
        classDeclaration();
    } else if (match(.TOKEN_VAR)) {
        varDeclaration();
    } else if (match(.TOKEN_FUN)) {
        functionDeclaration();
    } else {
        statement();
    }

    if (parser.panic_mode) {
        synchronize();
    }
}

fn statement() void {
    if (match(.TOKEN_PRINT)) {
        printStatement();
    } else if (match(.TOKEN_FOR)) {
        forStatement();
    } else if (match(.TOKEN_IF)) {
        ifStatement();
    } else if (match(.TOKEN_RETURN)) {
        returnStatement();
    } else if (match(.TOKEN_WHILE)) {
        whileStatement();
    } else if (match(.TOKEN_SWITCH)) {
        switchStatement();
    } else if (match(.TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

pub fn compile(source: []const u8) ?*object_.ObjFunction {
    scanner.init(source);

    var compiler = Compiler{};
    compiler.enclosing = null;
    compiler.init(.TYPE_SCRIPT);

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(.TOKEN_EOF)) {
        declaration();
    }
    const func = end_compiler();
    if (parser.had_error) {
        return null;
    } else {
        return func;
    }
}

pub fn markCompilerRoots() void {
    var compiler: ?*Compiler = current;
    while (compiler != null) : (compiler = compiler.?.enclosing) {
        gc_.markObject(&compiler.?.function.?.obj);
    }
}
