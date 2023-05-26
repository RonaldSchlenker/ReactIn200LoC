using System.IO;


async Task<string> ReadAll() {
    var content = await File.ReadAllTextAsync("file1.txt");
    var whatever = await File.ReadAllTextAsync("file2.txt");
    return (content + whatever);
}






























// |------ Task<string> -----|
async Task<string> ReadAll2() {

//      |string|  |-------------- Task<string> ------------|
    var content = await File.ReadAllTextAsync("file1.txt");
    
//      |string|   |------------- Task<string> ------------|
    var whatever = await File.ReadAllTextAsync("file2.txt");
    
//         |-------string------|
    return (content + whatever);
}
