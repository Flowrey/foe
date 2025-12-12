const std = @import("std");
const term = @import("term.zig");

const Allocator = std.mem.Allocator;

pub const Row = struct {
    const Self = @This();

    // Row index in the file, zero-based.
    idx: usize,
    /// Row content, dynamically allocated.
    chars: []u8,

    pub var empty: Self = .{ .idx = 0, .chars = undefined };
};

pub const Editor = struct {
    const Self = @This();

    /// Allocator used
    allocator: Allocator,
    /// The output writer
    stdout: *std.Io.Writer,
    /// Cursor x position in characters
    cx: usize,
    /// Cursor y position in characters
    cy: usize,
    /// Number of rows that we can show
    screenrows: usize,
    /// Number of cols thaht we can show
    screencols: usize,
    /// Rows
    rows: []Row,
    /// Number of rows
    numrows: usize,

    pub fn init(allocator: Allocator, stdout: *std.Io.Writer) Self {
        return .{
            .allocator = allocator,
            .stdout = stdout,
            .cx = 0,
            .cy = 0,
            .screenrows = 0,
            .screencols = 0,
            .rows = undefined,
            .numrows = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.rows) |row| {
            self.allocator.free(row.chars);
        }
        self.allocator.free(self.rows);
    }

    pub fn appendRow(self: *Self, s: []const u8) !void {
        // FIXME: Use a capacity to avoid realloc every time
        self.rows = try self.allocator.realloc(self.rows, self.numrows + 1);
        self.rows[self.numrows] = .{ .idx = self.numrows, .chars = try self.allocator.alloc(u8, s.len) };
        @memcpy(self.rows[self.numrows].chars, s);
        self.numrows += 1;
    }

    pub fn open(self: *Self, filename: []const u8) !void {
        const cwd = std.fs.cwd();

        const file = try cwd.openFile(filename, .{ .mode = .read_only });
        defer file.close();

        self.rows = try self.allocator.alloc(Row, 1);

        // FIXME: This is really not memory efficient!
        const content = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024); // 10MB max
        defer self.allocator.free(content);

        var lines = std.mem.splitSequence(u8, content, "\n");
        while (lines.next()) |line| {
            try self.appendRow(line);
        }
    }

    pub fn run(self: *Self) !void {
        while (true) {
            try self.refreshScreen();
            const c = try readKey();
            switch (c) {
                'q' => break, // Quit
                'h' => {
                    if (self.cx != 0) {
                        self.cx -= 1; // Move left
                    }
                },
                'j' => {
                    if (self.cy != self.screenrows - 1) {
                        self.cy += 1; // Mode down
                    }
                },
                'k' => {
                    if (self.cy != 0) {
                        self.cy -= 1; // Move up
                    }
                },
                'l' => {
                    if (self.cx != self.screencols - 1) {
                        self.cx += 1; // Move right
                    }
                },
                else => continue,
            }
        }
    }

    fn readKey() !u8 {
        var nread: usize = 0;
        var c: u8 = undefined;
        const buf: []u8 = std.mem.asBytes(&c);
        while ((nread != 1)) {
            nread = try std.posix.read(std.posix.STDIN_FILENO, buf);
        }
        return c;
    }

    fn drawRows(self: *Self) !void {
        var y: usize = 0;
        while (y < self.screenrows) {
            const filerow: usize = y;
            // If we are past the row buffer we write `~`
            if (y >= self.numrows) {
                try self.stdout.writeAll("~");
            } else { // Else we write the row
                try self.stdout.writeAll(self.rows[filerow].chars);
            }

            try term.eraseCurrentLine(self.stdout);
            if (y < self.screenrows - 1) {
                try self.stdout.writeAll("\r\n");
            }

            y += 1;
        }
    }

    fn refreshScreen(self: *Self) !void {
        try term.hideCursor(self.stdout); // hide cursor
        try term.resetCursorPosition(self.stdout); // set the cursor at the beginning of the file

        try self.drawRows(); // write the actual file rows

        try term.setCursorPosition(self.stdout, self.cx + 1, self.cy + 1); // set cursor position

        try term.showCursor(self.stdout); // show cursor
        try self.stdout.flush(); // flush the buffer
    }
};
