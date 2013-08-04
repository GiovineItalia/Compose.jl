
/* Use freetype to read a font and dump typeface name and glyph sizes for
 * printable ascii characters into an easily parsible JSON format. */

#include <stdio.h>

#include <ft2build.h>
#include FT_FREETYPE_H


/* Write a font's glyph extents to stdout. */
void dumpfont(FT_Library library, const char* fn)
{
    FT_Error error;
    FT_Face face;
    error = FT_New_Face(library, fn, 0, &face);
    if (error) {
        fprintf(stderr, "Error reading font frome %s.\n", fn);
        exit(1);
    }

    /* set dpi to micrometer per inch, so sizes are reported in micrometers. */
    const FT_UInt dpi = 25400;
    error = FT_Set_Char_Size(face, 0, 12*64, dpi, dpi);
    if (error) {
        fprintf(stderr, "Error setting font size.\n");
        exit(1);
    }

    printf("\"%s\": {\n  \"widths\": {\n  ", face->family_name);

    char c;
    FT_UInt i;
    FT_Pos max_height = 0;
    for (c = 0x20; c <= 0x7e; ++c) {
        i = FT_Get_Char_Index(face, c);
        error = FT_Load_Glyph(face, i, FT_LOAD_DEFAULT);
        if (error) {
            fprintf(stderr, "Error loading glyph for '%c'.", c);
            exit(1);
        }

        if (face->glyph->metrics.vertAdvance > max_height) {
            max_height = face->glyph->metrics.vertAdvance;
        }

        printf("  \"");
        if (c == '"' || c == '\\') putchar('\\');
        printf("%c\": ", c);
        if (c != '"' && c != '\\') putchar(' ');
        printf("%5.2f", (double) face->glyph->advance.x / 64.0 / 1000.0);
        if (c != 0x7e) putchar(',');
        if ((c - 0x20 + 1) % 5 == 0) {
            printf("\n  ");
        }
    }
    printf("},\n  \"height\": %0.2f\n}", (double) max_height / 64.0 / 1000.0);


    FT_Done_Face(face);
}


int main(int argc, char* argv[])
{
    if (argc < 2) {
        fprintf(stderr, "Usage: glyphsize fontfile [fontfile2...] > stuff.json\n");
        return 1;
    }

    FT_Library library;
    FT_Error error;

    error = FT_Init_FreeType(&library);
    if (error) {
        fprintf(stderr, "Error initalizing freetype.\n");
        return 1;
    }

    puts("{");
    int i;
    for (i = 1; i < argc; ++i) {
        dumpfont(library, argv[i]);
        if (i != argc - 1) puts(",");
    }
    puts("}");

    FT_Done_FreeType(library);

    return 0;
}
