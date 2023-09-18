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
};

const Compiler = struct {
    locals: [256]Local = undefined,
    local_count: u8 = 0,
    scope_depth: i32 = 0,

    fn init(self: *Compiler) void {
        current = self;
    }
};

var parser: Parser = undefined;
var scanner: Scanner = undefined;
var current: *Compiler = undefined;
var compiling_chunk: *Chunk = undefined;

inline fn current_chunk() *Chunk {
    return compiling_chunk;
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

fn end_compiler() void {
    emit_return();
    if (common.debug_print_code) {
        if (!parser.had_error)
            current_chunk().disassemble("code");
    }
}

inline fn beginScope() void {
    current.scope_depth += 1;
}

inline fn endScope() void {
    current.scope_depth -= 1;
    var pop_n: u8 = 0;
    var i = @as(i32, current.local_count) - 1;
    while (i >= 0 and current.locals[@intCast(i)].depth > current.scope_depth) : (i -= 1) {
        pop_n += 1;
    }
    current.local_count -= pop_n;
    if (pop_n > 0) {
        emit_op(.OP_POPN);
        emit_byte(pop_n);
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
    emit_constant(Value{ .Number = value });
}

fn or_(_: bool) void {
    const jump = emitJump(.OP_JUMP_IF_TRUE);

    emit_op(.OP_POP);
    parse_precedence(.PREC_OR);

    patchJump(jump);
}

fn string(_: bool) void {
    const sub_obj = object_.ObjString.copy(parser.previous.lexeme[1 .. parser.previous.lexeme.len - 1]);
    emit_constant(Value{ .Obj = &sub_obj.obj });
}

fn namedVariable(name: Token, can_assign: bool) void {
    var set_op: OpCode = undefined;
    var get_op: OpCode = undefined;

    var arg = resolveLocal(current, &name);
    if (arg != null) {
        set_op = .OP_SET_LOCAL;
        get_op = .OP_GET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        set_op = .OP_SET_GLOBAL;
        get_op = .OP_GET_GLOBAL;
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

    arr_[@intFromEnum(TokenType.TOKEN_LEFT_PAREN)] = ParseRule{ .prefix = grouping };
    arr_[@intFromEnum(TokenType.TOKEN_RIGHT_PAREN)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_LEFT_BRACE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_RIGHT_BRACE)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_COMMA)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_DOT)] = ParseRule{};
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
    arr_[@intFromEnum(TokenType.TOKEN_SUPER)] = ParseRule{};
    arr_[@intFromEnum(TokenType.TOKEN_THIS)] = ParseRule{};
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
    return make_constant(Value{ .Obj = &object_.ObjString.copy(name.lexeme).obj });
}

fn addLocal(name: Token) void {
    if (current.local_count == 256) {
        error_("Too many local variables in a function.");
        return;
    }
    var local = &current.locals[current.local_count];
    current.local_count += 1;
    local.name = name;
    local.depth = -1;
}

fn declareVariable() void {
    if (current.scope_depth == 0) return;

    const name = &parser.previous;
    var i = @as(i32, current.local_count) - 1;
    while (i >= 0) : (i -= 1) {
        var local = &current.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < current.scope_depth) {
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
    if (current.scope_depth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn markInitialized() void {
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn defineVariable(global: u8) void {
    if (current.scope_depth > 0) {
        markInitialized();
        return;
    }
    emit_op(.OP_DEFINE_GLOBAL);
    emit_byte(global);
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

fn declaration() void {
    if (match(.TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }
}

fn statement() void {
    if (match(.TOKEN_PRINT)) {
        printStatement();
    } else if (match(.TOKEN_FOR)) {
        forStatement();
    } else if (match(.TOKEN_IF)) {
        ifStatement();
    } else if (match(.TOKEN_WHILE)) {
        whileStatement();
    } else if (match(.TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

pub fn compile(source: []const u8, chunk: *Chunk) bool {
    scanner.init(source);

    var compiler = Compiler{};
    compiler.init();

    compiling_chunk = chunk;
    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(.TOKEN_EOF)) {
        declaration();
    }
    end_compiler();
    return !parser.had_error;
}
