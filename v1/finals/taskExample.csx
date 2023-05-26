
Task<string> ReadConsole() => throw new Exception("impl");

Task<string> ReadStream() => throw new Exception("impl");

async Task<string> ReadAll() {
    var console = await ReadConsole();
    var whateverMessage = await ReadStream();
    return (console + whateverMessage);
}
