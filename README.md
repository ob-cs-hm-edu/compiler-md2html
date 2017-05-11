# Ein Markdown-To-HTML-Compiler

    md2html

    Usage: md2html COMMAND [-o|--output OUTFILE] INFILE
      Markdown-To-HTML-Compiler

    Available options:
      -o,--output OUTFILE      Ausgabe in Datei umleiten
      INFILE                   Quell-Datei im Markdown-Format
      -h,--help                Show this help text

    Available commands:
      tokens                   gescannte Tokens ausgeben
      ast                      durch den Parser erzeugten AST ausgeben
      html                     HTML erzeugen
      dot                      AST in dot-Syntax ausgeben
