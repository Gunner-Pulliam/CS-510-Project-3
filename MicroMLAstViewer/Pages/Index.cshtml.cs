using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using MicroMLParser; // This references the F# namespace

public class IndexModel : PageModel
{
    [BindProperty]
    public string CodeInput { get; set; } = "";  // Fix CS8618 by setting default

    public string AstJson { get; set; } = "";    // Fix CS8618 by setting default

    public void OnGet()
    {
        // Initialize with optional default input if desired
    }

    public void OnPost()
    {
        if (!string.IsNullOrWhiteSpace(CodeInput))
        {
            AstJson = DummyParser.parseToJson(CodeInput);  // F# call
        }
    }
}