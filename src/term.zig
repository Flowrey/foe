const std = @import("std");

pub inline fn hideCursor(stdout: *std.Io.Writer) error{WriteFailed}!void {
    return stdout.writeAll("\x1b[?25l");
}

pub inline fn showCursor(stdout: *std.Io.Writer) error{WriteFailed}!void {
    return stdout.writeAll("\x1b[?25h");
}

pub inline fn resetCursorPosition(stdout: *std.Io.Writer) error{WriteFailed}!void {
    return stdout.writeAll("\x1b[H");
}

pub inline fn setCursorPosition(stdout: *std.Io.Writer, x: usize, y: usize) error{WriteFailed}!void {
    return stdout.print("\x1b[{d};{d}H", .{ y, x });
}

pub inline fn eraseEntireScreen(stdout: *std.Io.Writer) error{WriteFailed}!void {
    return stdout.writeAll("\x1b[2J");
}

pub inline fn eraseCurrentLine(stdout: *std.Io.Writer) error{WriteFailed}!void {
    return stdout.writeAll("\x1b[K");
}

pub fn enableRawMode(termios: std.posix.termios) !void {
    var raw = termios;
    raw.iflag.BRKINT = false;
    raw.iflag.ICRNL = false;
    raw.iflag.INPCK = false;
    raw.iflag.ISTRIP = false;
    raw.iflag.IXON = false;

    raw.oflag.OPOST = false;

    raw.cflag.CSIZE = .CS8;

    raw.lflag.ECHO = false;
    raw.lflag.ICANON = false;
    raw.lflag.IEXTEN = false;
    raw.lflag.ISIG = false;

    raw.cc[@intFromEnum(std.posix.system.V.MIN)] = 0;
    raw.cc[@intFromEnum(std.posix.system.V.TIME)] = 1;
    try std.posix.tcsetattr(std.posix.STDIN_FILENO, std.posix.TCSA.FLUSH, raw);
}

pub fn getWindowSize() std.posix.winsize {
    const TIOCGWINSZ = 0x5413;
    var ws: std.posix.winsize = undefined;
    _ = std.os.linux.ioctl(std.posix.STDOUT_FILENO, TIOCGWINSZ, @intFromPtr(&ws));
    return ws;
}
