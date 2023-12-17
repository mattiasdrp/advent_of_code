Beam = Data.define(:row, :col, :dir) do
  private_constant

  def move(angle = dir)
    nbeam = with(dir: (angle % 360))
    case nbeam.dir
    in 0
      nbeam.with(col: nbeam.col + 1)
    in 90
      nbeam.with(row: nbeam.row - 1)
    in 180
      nbeam.with(col: nbeam.col - 1)
    in 270
      nbeam.with(row: nbeam.row + 1)
    else
    end
  end

  def vertical?
    dir == 90 || dir == 270
  end

  def horizontal?
    !vertical?
  end

  def out?(width, height)
    row.negative? || row == height || col.negative? || col == width
  end

  def pretty_print(pp)
    pp.pp [row, col, dir]
  end
end

def move_beam(beam, visited, new_beams, grid)
  return nil, new_beams if visited.include?(beam) || beam.out?(grid[0].length, grid.length)

  visited.add(beam)
  case grid[beam.row][beam.col]
  in "."
    [beam.move, new_beams]
  in "|" if beam.vertical?
    [beam.move, new_beams]
  in "-" if beam.horizontal?
    [beam.move, new_beams]
  in "|" | "-"
    nbeam = beam.move(beam.dir + 90)
    [beam.move(beam.dir - 90), new_beams.add(nbeam)]
  in "/"
    angle =
      case beam.dir
      in 270
        180
      in 180
        270
      in 0
        90
      in 90
        0
      end
    [beam.move(angle), new_beams]
  in "\\"
    angle =
      case beam.dir
      in 270
        0
      in 180
        90
      in 0
        270
      in 90
        180
      end
    [beam.move(angle), new_beams]
  else
  end
end

def move_until_end(beam, visited, new_beams, grid)
  beam, new_beams = move_beam(beam, visited, new_beams, grid) until beam.nil?
  [visited, new_beams]
end

def pp_visited(visited, grid)
  grid.each_with_index do |line, row|
    line.each_with_index do |_cell, col|
      if visited.any? { |beam| beam.row == row && beam.col == col }
        print "#"
      else
        print "."
      end
    end
    puts
  end
end

def count_visited(visited)
  visited.uniq { [_1.row, _1.col] }.length
end

def simulate(beam, grid)
  visited = Set[]
  new_beams = Set[beam]
  until new_beams.empty?
    beam = new_beams.first
    new_beams = new_beams.delete(beam)
    visited, new_beams = move_until_end(beam, visited, new_beams, grid)
  end
  count_visited(visited)
end

def part1(file)
  grid = File.readlines(file, chomp: true).map(&:chars)
  simulate(Beam[row: 0, col: 0, dir: 0], grid)
end

def max(val1, val2)
  val1 > val2 ? val1 : val2
end

def part2(file)
  grid = File.readlines(file, chomp: true).map(&:chars)
  width = grid[0].length - 1
  height = grid.length - 1
  res = (0..height).reduce(0) do |res, row|
    res = max(res, simulate(Beam[row:, col: 0, dir: 0], grid))
    max(res, simulate(Beam[row:, col: width, dir: 180], grid))
  end
  (0..width).reduce(res) do |res, col|
    res = max(res, simulate(Beam[row: 0, col:, dir: 270], grid))
    max(res, simulate(Beam[row: height, col:, dir: 90], grid))
  end
end

pp(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
