
require 'fileutils'

SCALA_ROOT = File.expand_path("../../../scala", __FILE__)

# Writers

def write_file(package, f_name, str)
  dir = File.join(SCALA_ROOT, package.tr(".", "/"))
  FileUtils.mkdir_p(dir)
  path = File.join(dir, "#{f_name}.scala")
  File.open(path, "w") do |f|
    f.puts("// DO NOT EDIT : This file was automatically generated")
    f.puts("package #{package}")
    f.puts
    f.write(str)
  end
end

def write_fragment(package, f_name, str)
  path = File.join(SCALA_ROOT, package.tr(".", "/"), "#{f_name}.scala")
  puts(path)
  contents = File.read(path)
  matched = /^(.*\/\/ codegen-fragment-start)(.*)(\n([ \t]*)\/\/ codegen-fragment-end.*)$/m.match(contents)
  File.open(path, "w") do |f|
    f.write(matched[1])
    f.write("\n")
    f.write("#{matched[4]}#{str.gsub("\n", "\n#{matched[4]}")}")
    f.write(matched[3])
  end
end

# Contents

def applicative(min, max)
  entry = proc do |o|
    list_o_types = (1..o).map { |o2| "\n  T#{o2}," }.join
    list_o_args = (1..o).map { |o2| "\n _#{o2}: T[T#{o2}]," }.join
    join_rec = proc do |o2|
      if o2 <= 1
        "_#{o2}"
      else
        "#{join_rec.(o2 - 1)}.aJoin(_#{o2})"
      end
    end
    match_rec = proc do |o2|
      if o2 <= 2
        "(_#{o2 - 1}, _#{o2})"
      else
        "(#{match_rec.(o2 - 1)}, _#{o2})"
      end
    end
<<-SCALA
def join[#{list_o_types}
](#{list_o_args}
): T[(#{list_o_types}
)] =
 #{join_rec.(o)}.map {
    case #{match_rec.(o)} =>
      (#{(1..o).map { |o2| "_#{o2}" }.join(", ")})
  }
SCALA
  end

<<-SCALA
// format: off
import klib.fp.typeclass.Applicative
import klib.fp.typeclass.Implicits._

final class ado[T[_]: Applicative] {

  #{(min..max).map(&entry).join("\n").gsub("\n", "\n  ")}
}

object ado {

  def apply[T[_]: Applicative]: ado[T] =
    new ado[T]

}

// format: on
SCALA
end

write_file("klib.fp.utils", "ado", applicative(2, 22))
