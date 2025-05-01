const std = @import("std");
const root = @import("root");
const Token = root.Token;
const termcolor = root.termcolor;

pub const ReportKind = enum(u8) {
    Error,
    Warning,
    Info,

    pub fn to_string(self: ReportKind) []const u8 {
        return switch (self) {
            .Error => termcolor.ANSI_CODE_BOLD ++ termcolor.ANSI_CODE_RED ++ "Error" ++ termcolor.ANSI_CODE_RESET,
            .Warning => termcolor.ANSI_CODE_BOLD ++ termcolor.ANSI_CODE_YELLOW ++ "Warning" ++ termcolor.ANSI_CODE_RESET,
            .Info => termcolor.ANSI_CODE_BOLD ++ termcolor.ANSI_CODE_CYAN ++ "Info" ++ termcolor.ANSI_CODE_RESET,
        };
    }
};

pub const Caret = struct {
    msg: []const u8,
    caret: []const u8 = "^",
    column: usize = 0,
};

pub fn report_pro(
    content: []const u8,
    path: []const u8,
    caret: Caret,
    at: usize,
    line: usize,
    column: usize,
    kind: ReportKind,
    comptime fmt: []const u8,
    args: anytype,
) void {
    const lines = get_line(content, at);

    std.debug.print("{s}{s}{s}:{}:{}:{s} {s}: ", .{
        termcolor.ANSI_CODE_UNDERLINE,
        termcolor.ANSI_CODE_BOLD,
        path,
        line,
        column,
        termcolor.ANSI_CODE_RESET,
        kind.to_string(),
    });

    std.debug.print(fmt, args);

    const line_chars_width = number_chars_len(usize, line);
    std.debug.print("\n {} | {s}\n", .{ line, lines });

    std.debug.print("{s:>[1]} {2s}\n", .{
        caret.caret,
        caret.column + 4 + line_chars_width,
        caret.msg,
    });
}

fn get_line(content: []const u8, index: usize) []const u8 {
    if (index >= content.len)
        return "";

    var start = index;
    var end = index;

    while (start > 0) : (start -= 1) {
        if (content[start - 1] == '\n')
            break;
    }

    while (end < content.len) : (end += 1) {
        if (content[end] == '\n')
            break;
    }

    return content[start..end];
}

fn number_chars_len(T: type, num: T) usize {
    var count: usize = 0;
    var n = num;

    while (n != 0) : (n /= 10) {
        count += 1;
    }

    return count;
}
