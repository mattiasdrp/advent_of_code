# frozen_string_literal: true

def min(val1, val2)
  return val1 if val2.nil?

  val1 < val2 ? val1 : val2
end

def range1((row, col))
  if row.zero?
    (col < 3 * col ? (col..3 * col) : (3 * col..col)).map do
      [[0, _1], [row, col]]
    end
  else
    (row < 3 * row ? (row..3 * row) : (3 * row..row)).map { [[_1, 0], [row, col]] }
  end
end

def range2((row, col))
  if row.zero?
    (4 * col < 10 * col ? (4 * col..10 * col) : (10 * col..4 * col)).map do
      [[0, _1], [row, col]]
    end
  else
    (4 * row < 10 * row ? (4 * row..10 * row) : (10 * row..4 * row)).map { [[_1, 0], [row, col]] }
  end
end

def range(delta, part)
  part == 1 ? range1(delta) : range2(delta)
end

Cell = Data.define(:row, :col, :from) do
  DIRECTIONS = [[-1, 0], [0, 1], [1, 0], [0, -1]].freeze

  def opposite((row, col))
    [-row, -col]
  end

  def neighbours(grid, part)
    dirs = DIRECTIONS - [from, opposite(from)]
    # dirs = count == 3 ? dirs - [opposite(from)] : dirs
    dirs.flat_map { range(_1, part) }.filter_map do |((prow, pcol), from)|
      nrow = row + prow
      ncol = col + pcol
      next unless nrow >= 0 && nrow < grid.length && ncol >= 0 && ncol < grid[0].length

      value =
        if prow.zero?
          if pcol.positive?
            (1..pcol).sum(0) { grid[row][col + _1] }
          else
            (pcol..-1).sum(0) { grid[row][col + _1] }
          end
        elsif prow.positive?
          (1..prow).sum(0) { grid[row + _1][col] }
        else
          (prow..-1).sum(0) { grid[row + _1][col] }
        end
      [with(
        row: nrow,
        col: ncol,
        from:
      ), value]
    end
  end

  def pretty_print(pp)
    prow, pcol = from
    prow = row + prow
    pcol = col + pcol
    pp.pp [[prow, pcol], [row, col]]
  end
end

require "fc"

def dijkstra(grid, part)
  visited = {}
  start = Cell[row: 0, col: 0, from: [0, 0]]
  distances = { start => 0 }
  queue = FastContainers::PriorityQueue.new(:min)
  queue.push(start, 0)

  until queue.empty?
    min_value = queue.top_key
    min_cell = queue.pop
    next if visited.include?(min_cell)
    # puts "#{min_cell} #{min_value}"
    return min_value if min_cell.row == grid.length - 1 && min_cell.col == grid[0].length - 1

    # For each neighboring node of the current node
    min_cell.neighbours(grid, part).each do |(neighbour, value)|
      curr_dist = distances[neighbour]
      alt = min_value + value
      dist = min(alt, curr_dist)
      distances[neighbour] = dist
      queue.push(neighbour, dist)
    end

    # Mark the node as visited
    visited[min_cell] = min_value
  end
end

def part(file, part)
  grid = File.readlines(file, chomp: true).map { _1.chars.map(&:to_i) }
  dijkstra(grid, part)
end

puts(ARGV[1] == "1" ? part(ARGV[0], 1) : part(ARGV[0], 2))
