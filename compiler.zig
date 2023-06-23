const std = @import("std");

const common = @import("common.zig");
const scanner_ = @import("scanner.zig");
const Scanner = scanner_.Scanner;
const TokenType = scanner_.TokenType;
const chunk_ = @import("chunk.zig");
const Chunk = chunk_.Chunk;
const OpCode = chunk_.OpCode;
const value_ = @import("value.zig");
const Value = value_.Value;

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

var parser: Parser = undefined;
var scanner: Scanner = undefined;
var compiling_chunk: *Chunk = undefined;

inline fn current_chunk() *Chunk {
    return compiling_chunk;
}

fn error_at(token: scanner_.Token, msg: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    std.debug.print("[line {}] Error ", .{token.line});

    if (token.type == .TOKEN_EOF) {
        std.debug.print(" at end", .{});
    } else if (token.type == .TOKEN_ERROR) {} else std.debug.print(" at {s}", .{token.lexeme});
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

inline fn emit_bytes(byte1: u8, byte2: u8) void {
    emit_byte(byte1);
    emit_byte(byte2);
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
    return @intCast(u8, constant);
}

fn emit_constant(value: Value) void {
    emit_op(.OP_CONSTANT);
    emit_byte(make_constant(value));
}

fn end_compiler() void {
    emit_return();
    if (common.debug_print_code) {
        if (!parser.had_error)
            current_chunk().disassemble("code");
    }
}

fn binary(can_assign: bool) void {
    _ = can_assign;
    const token_type = parser.previous.type;
    const rule = get_rule(token_type);
    parse_precedence(@intToEnum(Precedence, @enumToInt(rule.precedence) + 1));

    switch (token_type) {
        .TOKEN_BANG_EQUAL => {
            emit_op(.OP_EQUAL);
            emit_op(.OP_NOT);
        },
        .TOKEN_EQUAL_EQUAL => emit_op(.OP_EQUAL),
        .TOKEN_GREATER => emit_op(.OP_GREATER),
        .TOKEN_GREATER_EQUAL => {
            emit_op(.OP_LESS);
            emit_op(.OP_NOT);
        },
        .TOKEN_LESS => emit_op(.OP_LESS),
        .TOKEN_LESS_EQUAL => {
            emit_op(.OP_GREATER);
            emit_op(.OP_NOT);
        },
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

    arr_[@enumToInt(TokenType.TOKEN_LEFT_PAREN)] = ParseRule{ .prefix = grouping };
    arr_[@enumToInt(TokenType.TOKEN_RIGHT_PAREN)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_LEFT_BRACE)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_RIGHT_BRACE)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_COMMA)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_DOT)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_MINUS)] = ParseRule{ .prefix = unary, .infix = binary, .precedence = .PREC_TERM };
    arr_[@enumToInt(TokenType.TOKEN_PLUS)] = ParseRule{ .infix = binary, .precedence = .PREC_TERM };
    arr_[@enumToInt(TokenType.TOKEN_SEMICOLON)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_SLASH)] = ParseRule{ .infix = binary, .precedence = .PREC_FACTOR };
    arr_[@enumToInt(TokenType.TOKEN_STAR)] = ParseRule{ .infix = binary, .precedence = .PREC_FACTOR };
    arr_[@enumToInt(TokenType.TOKEN_BANG)] = ParseRule{ .prefix = unary };
    arr_[@enumToInt(TokenType.TOKEN_BANG_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_EQUALITY };
    arr_[@enumToInt(TokenType.TOKEN_EQUAL)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_EQUAL_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_EQUALITY };
    arr_[@enumToInt(TokenType.TOKEN_GREATER)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@enumToInt(TokenType.TOKEN_GREATER_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@enumToInt(TokenType.TOKEN_LESS)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@enumToInt(TokenType.TOKEN_LESS_EQUAL)] = ParseRule{ .infix = binary, .precedence = .PREC_COMPARISON };
    arr_[@enumToInt(TokenType.TOKEN_IDENTIFIER)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_STRING)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_NUMBER)] = ParseRule{ .prefix = number };
    arr_[@enumToInt(TokenType.TOKEN_AND)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_CLASS)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_ELSE)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_FALSE)] = ParseRule{ .prefix = literal };
    arr_[@enumToInt(TokenType.TOKEN_FOR)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_FUN)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_IF)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_NIL)] = ParseRule{ .prefix = literal };
    arr_[@enumToInt(TokenType.TOKEN_OR)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_PRINT)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_RETURN)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_SUPER)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_THIS)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_TRUE)] = ParseRule{ .prefix = literal };
    arr_[@enumToInt(TokenType.TOKEN_VAR)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_WHILE)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_ERROR)] = ParseRule{};
    arr_[@enumToInt(TokenType.TOKEN_EOF)] = ParseRule{};
    return arr_;
}
const rules = make_rules();

inline fn get_rule(type_: TokenType) *const ParseRule {
    return &rules[@enumToInt(type_)];
}

fn parse_precedence(precedence: Precedence) void {
    advance();

    const token_type = parser.previous.type;
    if (rules[@enumToInt(token_type)].prefix) |rule| {
        rule(false);
    } else {
        error_("Expect expression.");
    }

    const precedence_int = @enumToInt(precedence);
    while (precedence_int <= @enumToInt(get_rule(parser.current.type).precedence)) {
        advance();
        const infix_rule = get_rule(parser.previous.type).infix.?;
        infix_rule(false);
    }
}

fn expression() void {
    parse_precedence(.PREC_ASSIGNMENT);
}

pub fn compile(source: []const u8, chunk: *Chunk) bool {
    scanner.init(source);
    compiling_chunk = chunk;
    parser.had_error = false;
    parser.panic_mode = false;

    advance();
    expression();
    consume(.TOKEN_EOF, "Expect end of expression.");
    end_compiler();
    return !parser.had_error;
}
