Galaxies = Data.define(:rows, :cols)

def parse(file)
  map = []
  galaxies = Galaxies[rows: Set[], cols: Set[]]
  File.readlines(file, chomp: true).each_with_index do |line, row|
    line.each_char.with_index do |char, col|
      next unless char == "#"

      map << [row, col]
      galaxies.rows.add(row)
      galaxies.cols.add(col)
    end
  end
  [map, galaxies]
end

def manhattan_distance(galaxies, grow1, gcol1, grow2, gcol2, space)
  grow1, grow2 = grow2, grow1 if grow1 > grow2
  gcol1, gcol2 = gcol2, gcol1 if gcol1 > gcol2
  space * (grow1..grow2).count { !galaxies.rows.include?(_1) } +
    space * (gcol1..gcol2).count { !galaxies.cols.include?(_1) } +
    grow2 - grow1 + gcol2 - gcol1
end

def part(file, space)
  space -= 1
  map, galaxies = parse(file)
  map.combination(2).sum do |galaxy1, galaxy2|
    manhattan_distance(galaxies, *galaxy1, *galaxy2, space)
  end
end

puts(ARGV[1] == "1" ? part(ARGV[0], 2) : part(ARGV[0], 1_000_000))
