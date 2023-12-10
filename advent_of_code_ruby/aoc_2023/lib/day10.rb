def parse(file)
  map = {}
  start = []
  File.readlines(file, chomp: true).each_with_index do |line, row|
    line.each_char.with_index do |char, col|
      case char
      in "|"
        map[[row, col]] = [[row - 1, col], [row + 1, col]]
      in "-"
        map[[row, col]] = [[row, col - 1], [row, col + 1]]
      in "L"
        map[[row, col]] = [[row - 1, col], [row, col + 1]]
      in "J"
        map[[row, col]] = [[row - 1, col], [row, col - 1]]
      in "7"
        map[[row, col]] = [[row + 1, col], [row, col - 1]]
      in "F"
        map[[row, col]] = [[row + 1, col], [row, col + 1]]
      in "S"
        start = [row, col]
        map[[row, col]] = [[row - 1, col], [row, col + 1], [row + 1, col], [row, col - 1]]
      else
      end
    end
  end
  [start, map]
end

def loop?(end_loop, start, map)
  prev = end_loop
  length = 0
  until start.eql?(end_loop)
    length += 1
    return nil if map[start].nil?

    dest = (map[start] - [prev]).flatten
    prev = start
    start = dest
  end
  length
end

def complete_graph((start, map))
  destinations = map[start]
  dest = destinations.find { |dest| loop?(start, dest, map) }
  ((loop?(start, dest, map) + 1) / 2)
end

p complete_graph(parse(ARGV[0]))
