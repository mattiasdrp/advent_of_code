# frozen_string_literal: true

def min(val1, val2)
  return val1 if val2.nil?

  val1 < val2 ? val1 : val2
end

def range((row, col))
  if row.zero?
    (col < 3 * col ? (col..3 * col) : (3 * col..col)).map do
      [[0, _1], [row, col]]
    end
  else
    (row < 3 * row ? (row..3 * row) : (3 * row..row)).map { [[_1, 0], [row, col]] }
  end
end

Cell = Data.define(:row, :col, :from) do
  DIRECTIONS = [[-1, 0], [0, 1], [1, 0], [0, -1]].freeze

  def opposite((row, col))
    [-row, -col]
  end

  def neighbours(grid)
    dirs = DIRECTIONS - [from, opposite(from)]
    # dirs = count == 3 ? dirs - [opposite(from)] : dirs
    dirs.flat_map { range(_1) }.filter_map do |((prow, pcol), from)|
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

# require "fc"

def dijkstra(grid)
  visited = {}
  start = Cell[row: 0, col: 0, from: [0, 0]]
  queue = { start => 0 }

  until queue.empty?
    min_cell, min_value = queue.min_by { |_, value| value }
    # puts "#{min_cell} #{min_value}"
    return min_value if min_cell.row == grid.length - 1 && min_cell.col == grid[0].length - 1

    # For each neighboring node of the current node
    min_cell.neighbours(grid).each do |(neighbour, value)|
      # puts "#{neighbour} val = #{value}"
      # pp neighbour
      curr_dist = queue[neighbour]
      alt = min_value + value
      queue[neighbour] = min(alt, curr_dist)
    end

    # Mark the node as visited
    visited[min_cell] = min_value
    queue.delete(min_cell)
    # puts "visited"
    # pp visited
    # puts "queue"
    # pp queue
  end
end

def part1(file)
  grid = File.readlines(file, chomp: true).map { _1.chars.map(&:to_i) }
  pp dijkstra(grid)
end

part1(ARGV[0])
