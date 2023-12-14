# frozen_string_literal: true

Cell = Data.define(:type, :dirs)

def parse(file)
  map = {}
  start = []
  nb_lines = 0
  File.readlines(file, chomp: true).each_with_index do |line, row|
    nb_lines += 1
    line.each_char.with_index do |char, col|
      case char
      in "|"
        map[[row, col]] = Cell[type: :Pipe, dirs: [[row - 1, col], [row + 1, col]]]
      in "-"
        map[[row, col]] = Cell[type: :Continue, dirs: [[row, col - 1], [row, col + 1]]]
      in "L"
        map[[row, col]] = Cell[type: :L,  dirs: [[row - 1, col], [row, col + 1]]]
      in "J"
        map[[row, col]] = Cell[type: :J,  dirs: [[row - 1, col], [row, col - 1]]]
      in "7"
        map[[row, col]] = Cell[type: :Seven, dirs: [[row + 1, col], [row, col - 1]]]
      in "F"
        map[[row, col]] = Cell[type: :F, dirs: [[row + 1, col], [row, col + 1]]]
      in "S"
        start = [row, col]
        map[[row, col]] =
          Cell[type: :Start, dirs: [[row - 1, col], [row, col + 1], [row + 1, col], [row, col - 1]]]
      else
      end
    end
  end
  [start, map, nb_lines, File.readlines(file, chomp: true)[0].length]
end

def loop?(end_loop, start, map)
  prev = end_loop
  graph = {}
  until start.eql?(end_loop)
    return nil if map[start].nil?

    graph[start] = map[start].type
    dest = (map[start].dirs - [prev]).flatten
    prev = start
    start = dest
  end
  [graph, prev]
end

def complete_graph(start, map)
  destinations = map[start].dirs
  graph = {}
  from = nil
  dest = destinations.find do
    graph, from = loop?(start, _1, map)
    graph
  end
  graph[start] =
    if from[0] == dest[0]
      :Continue
    elsif from[1] == dest[1]
      :Pipe
    else
      from, dest = dest, from if from[1] > dest[1]
      if from[0] == start[0]
        if from[0] > dest[0]
          :J
        else
          :Seven
        end
      elsif from[0] > dest[0]
        :F
      else
        :L
      end
    end
  graph
end

def part1(file)
  start, map, = parse(file)
  graph = complete_graph(start, map)
  (graph.length + 1) / 2
end

def ray_casting(row, width, inside, graph)
  intersections = 0
  prev_border = :Continue
  (0..width - 1).each do |col|
    border = graph[[row, col]]
    case border
    in :Pipe | :L | :F
      intersections += 1
    in :Seven if prev_border != :L
      intersections += 1
    in :J if prev_border != :F
      intersections += 1
    else
    end
    inside << [row, col] if intersections.odd? & !border
    prev_border = border if border != :Continue
  end
end

def part2(file)
  start, map, height, width = parse(file)
  graph = complete_graph(start, map)
  inside = []
  (0..height).each do |row|
    ray_casting(row, width, inside, graph)
  end
  inside.length
end

p(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
