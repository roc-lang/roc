app [main!] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.1/7brgwR8YUqfeGrkdfF1V4GPPN7DnP7ftqub9KcrTEZtr.tar.zst" }
import cli.Stdout
Model := [].{
        line_column : Str, U64 -> { line : U64, column : U64 }
        line_column = |text, offset| {
                bytes = text.to_utf8()
                var $line = 1
                var $start = 0
                var $index = 0
                while $index < offset {
                        byte = bytes.get($index) ?? 0
                        crlf = byte == '\r' and bytes.get($index + 1) == Ok('\n') and $index + 1 < offset
                        if byte == '\n' or byte == '\r' {
                                $index = if (crlf) $index + 2 else $index + 1
                                $line = $line + 1
                                $start = $index
                        } else {
                                $index = $index + 1
                        }
                }
                { line: $line, column: offset - $start + 1 }
        }

}
main! = |_args| {
 input = "<root>text &amp; more</root>"
 var $positions = []
 var $index = 0.U64
 while $index < input.count_utf8_bytes() {
  if input.to_utf8().get($index) == Ok('<') { $positions = $positions.append(Model.line_column(input, $index)) }
  $index = $index + 1
 }
 Stdout.line!(Str.inspect($positions))
}
