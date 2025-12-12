const std = @import("std");

const term = @import("term.zig");
const editor = @import("editor.zig");
const Editor = editor.Editor;

pub fn main() !void {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writter = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_writter.interface;

    var arena = std.heap.DebugAllocator(.{}){};
    const allocator = arena.allocator();
    defer _ = arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    var ed = Editor.init(allocator, stdout);
    defer ed.deinit();

    // FIXME: Check for nargs
    try ed.open(args[1]);

    const termios = try std.posix.tcgetattr(std.posix.STDIN_FILENO);
    try term.enableRawMode(termios);

    const winsize = term.getWindowSize();
    ed.screencols = winsize.col;
    ed.screenrows = winsize.row;

    try ed.run();

    // Cleanup
    try term.eraseEntireScreen(stdout);
    try term.resetCursorPosition(stdout);
    try stdout.flush();

    try std.posix.tcsetattr(std.posix.STDIN_FILENO, std.posix.TCSA.NOW, termios);
}
