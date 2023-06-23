const std = @import("std");

pub const TokenType = enum {
    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,

    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // Keywords.
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,

    TOKEN_ERROR,
    TOKEN_EOF,
};

pub const Token = struct {
    type: TokenType,
    lexeme: []const u8,
    line: u32,
};

pub const Scanner = struct {
    start: [*]const u8,
    current: [*]const u8,
    end: usize,
    line: u32,

    pub fn init(self: *Scanner, source: []const u8) void {
        self.start = source.ptr;
        self.current = source.ptr;
        self.line = 1;
        self.end = @ptrToInt(source.ptr + source.len);
    }

    inline fn is_at_end(self: *Scanner) bool {
        return @ptrToInt(self.current) >= self.end;
    }

    pub inline fn advance(self: *Scanner) u8 {
        defer self.current += 1;
        return self.current[0];
    }

    inline fn peek(self: *Scanner) u8 {
        return self.current[0];
    }

    inline fn peek_next(self: *Scanner) u8 {
        if (self.is_at_end()) return 0;
        return self.current[1];
    }

    inline fn match(self: *Scanner, c: u8) bool {
        if (self.is_at_end()) return false;
        if (self.current[0] != c) return false;
        self.current += 1;
        return true;
    }

    inline fn skip_white_space(self: *Scanner) void {
        while (true) {
            const c = self.current[0];
            switch (c) {
                ' ', '\t', '\r' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peek_next() == '/') {
                        while (self.peek() != '\n' and !self.is_at_end()) _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn make_token(self: *const Scanner, type_: TokenType) Token {
        return Token{
            .type = type_,
            .lexeme = self.start[0 .. @ptrToInt(self.current) - @ptrToInt(self.start)],
            .line = self.line,
        };
    }

    fn error_token(self: *const Scanner, msg: []const u8) Token {
        return Token{
            .type = TokenType.TOKEN_ERROR,
            .lexeme = msg,
            .line = self.line,
        };
    }

    fn check_keyword(self: *const Scanner, start: usize, rest: []const u8, type_: TokenType) TokenType {
        if (std.mem.eql(u8, self.start[start .. start + rest.len], rest)) {
            return type_;
        }
        return .TOKEN_IDENTIFIER;
    }

    fn identifier_type(self: *Scanner) TokenType {
        switch (self.start[0]) {
            'a' => return self.check_keyword(1, "nd", .TOKEN_AND),
            'c' => return self.check_keyword(1, "lass", .TOKEN_CLASS),
            'e' => return self.check_keyword(1, "lse", .TOKEN_ELSE),
            'f' => {
                if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                    switch (self.start[1]) {
                        'a' => return self.check_keyword(2, "lse", .TOKEN_FALSE),
                        'o' => return self.check_keyword(2, "r", .TOKEN_FOR),
                        'u' => return self.check_keyword(2, "n", .TOKEN_FUN),
                        else => return .TOKEN_IDENTIFIER,
                    }
                }
            },
            'i' => return self.check_keyword(1, "f", .TOKEN_IF),
            'n' => return self.check_keyword(1, "il", .TOKEN_NIL),
            'o' => return self.check_keyword(1, "r", .TOKEN_OR),
            'p' => return self.check_keyword(1, "rint", .TOKEN_PRINT),
            'r' => return self.check_keyword(1, "eturn", .TOKEN_RETURN),
            's' => return self.check_keyword(1, "uper", .TOKEN_SUPER),
            't' => {
                if (@ptrToInt(self.current) - @ptrToInt(self.start) > 1) {
                    switch (self.start[1]) {
                        'h' => return self.check_keyword(2, "is", .TOKEN_THIS),
                        'r' => return self.check_keyword(2, "ue", .TOKEN_TRUE),
                        else => return .TOKEN_IDENTIFIER,
                    }
                }
            },
            'v' => return self.check_keyword(1, "ar", .TOKEN_VAR),
            'w' => return self.check_keyword(1, "hile", .TOKEN_WHILE),
            else => return .TOKEN_IDENTIFIER,
        }
        return .TOKEN_IDENTIFIER;
    }

    fn identifier(self: *Scanner) Token {
        while (is_alpha(self.peek()) or is_digit(self.peek())) _ = self.advance();
        return self.make_token(self.identifier_type());
    }

    fn number(self: *Scanner) Token {
        while (is_digit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and is_digit(self.peek_next())) {
            _ = self.advance();
            while (is_digit(self.peek())) _ = self.advance();
        }
        return self.make_token(.TOKEN_NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }

        if (self.is_at_end()) {
            return self.error_token("Unterminated string.");
        }

        _ = self.advance(); //closing "
        return self.make_token(.TOKEN_STRING);
    }

    pub fn scan_token(self: *Scanner) Token {
        self.skip_white_space();
        self.start = self.current;

        if (self.is_at_end()) return self.make_token(.TOKEN_EOF);

        const c = self.advance();
        if (is_alpha(c)) return self.identifier();
        if (is_digit(c)) return self.number();

        switch (c) {
            '(' => return self.make_token(.TOKEN_LEFT_PAREN),
            ')' => return self.make_token(.TOKEN_RIGHT_PAREN),
            '{' => return self.make_token(.TOKEN_LEFT_BRACE),
            '}' => return self.make_token(.TOKEN_RIGHT_BRACE),
            ';' => return self.make_token(.TOKEN_SEMICOLON),
            ',' => return self.make_token(.TOKEN_COMMA),
            '.' => return self.make_token(.TOKEN_DOT),
            '-' => return self.make_token(.TOKEN_MINUS),
            '+' => return self.make_token(.TOKEN_PLUS),
            '/' => return self.make_token(.TOKEN_SLASH),
            '*' => return self.make_token(.TOKEN_STAR),
            '!' => return self.make_token(if (self.match('=')) .TOKEN_BANG_EQUAL else .TOKEN_BANG),
            '=' => return self.make_token(if (self.match('=')) .TOKEN_EQUAL_EQUAL else .TOKEN_EQUAL),
            '<' => return self.make_token(if (self.match('=')) .TOKEN_LESS_EQUAL else .TOKEN_LESS),
            '>' => return self.make_token(if (self.match('=')) .TOKEN_GREATER_EQUAL else .TOKEN_GREATER),
            '"' => return self.string(),
            else => return self.error_token("Unexpected character."),
        }
    }
};

inline fn is_alpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c == '_');
}

inline fn is_digit(c: u8) bool {
    return c >= '0' and c <= '9';
}
