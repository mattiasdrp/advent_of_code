Node = Data.define(:name, :row, :col)

def max_paths(graph, start, dest)
  queue = [[start, 0, Set[]]]
  distances = []
  until queue.empty?
    node, dist, visited = queue.pop
    next distances << dist if node == dest

    neighbours = graph[node]
    case neighbours
    in nil
      puts node

      next
    in neighbours
      neighbours.each do |(node, distp)|
        next if visited.include?(node)

        queue << [node, dist + distp, visited.clone.add(node)]
      end
    end
  end
  distances.max
end

module Part1
  State = Data.define(:node, :nrow, :ncol, :visited)

  module_function

  def main(file)
    graph, start, dest = parse(file)
    max_paths(graph, start, dest)
  end

  def find_next_node(row, col, forest, visited)
    dist = 0
    loop do
      dist += 1
      visited.add([row, col])
      case forest[row][col]
      in ">"
        col += 1
      in "<"
        col -= 1
      in "^"
        row -= 1
      in "v"
        row += 1
      in "." if row == forest.length - 1
        break [row, col, dist, [], visited]
      in "."
        possibilities =
          [[1, 0, "^"], [-1, 0, "v"], [0, 1, "<"], [0, -1, ">"]]
          .filter_map do |pr, pc, no|
            nr = row + pr
            nc = col + pc
            [nr, nc] if !visited.include?([nr, nc]) &&
                        forest[nr][nc] != "#" && forest[nr][nc] != no
          end
        case possibilities
        in [npos]
          row, col = npos
        in a
          break [row, col, dist, a, visited]
        end
      end
    end
  end

  def parse(file)
    forest = File.readlines(file, chomp: true).map(&:chars)
    name = 0
    start = Node[name: 0, row: 0, col: 1]
    queue = [State[node: start, nrow: 1, ncol: 1, visited: Set[[0, 1]]]]
    graph = Hash.new { |h, k| h[k] = [] }
    map = { [0, 1] => start }
    nodes = Set[]
    until queue.empty?
      state = queue.pop
      next if nodes.include?([state.node, state.nrow, state.ncol])

      nrow, ncol, dist, neighbours = find_next_node(state.nrow, state.ncol, forest, state.visited)
      nnode = map[[nrow, ncol]].nil? ? Node[name: name += 1, row: nrow, col: ncol] : map[[nrow, ncol]]
      map[[nrow, ncol]] = nnode
      graph[state.node] << [nnode, dist]
      neighbours.each do
        queue << State[node: nnode, nrow: _1, ncol: _2, visited: state.visited.clone]
      end
      nodes << [state.node, state.nrow, state.ncol]

    end
    [graph, start, map[[forest.length - 1, forest[0].length - 2]]]
  end
end

module Part2
  State = Data.define(:node, :nrow, :ncol)

  module_function

  def main(file)
    graph, start, dest = parse(file)
    max_paths(graph, start, dest)
  end

  def find_next_node(prow, pcol, row, col, forest)
    dist = 0
    loop do
      dist += 1
      case forest[row][col]
      in "." if row == forest.length - 1
        break [row, col, dist, []]
      in "." | "v" | ">" | "<"
        possibilities =
          [[1, 0], [-1, 0], [0, 1], [0, -1]]
          .filter_map do |pr, pc|
            nr = row + pr
            nc = col + pc
            [nr, nc] if (prow != nr || pcol != nc) &&
                        forest[nr][nc] != "#"
          end
        case possibilities
        in [npos]
          prow = row
          pcol = col
          row, col = npos
        in a
          break [row, col, dist, a]
        end
      end
    end
  end

  def parse(file)
    forest = File.readlines(file, chomp: true).map(&:chars)
    name = 0
    start = Node[name: 0, row: 0, col: 1]
    queue = [State[node: start, nrow: 1, ncol: 1]]
    graph = Hash.new { |h, k| h[k] = [] }
    map = { [0, 1] => start }
    nodes = Set[]
    until queue.empty?
      state = queue.pop
      next if nodes.include?([state.node, state.nrow, state.ncol])

      nrow, ncol, dist, neighbours =
        find_next_node(state.node.row, state.node.col, state.nrow, state.ncol, forest)
      nnode = map[[nrow, ncol]].nil? ? Node[name: name += 1, row: nrow, col: ncol] : map[[nrow, ncol]]
      map[[nrow, ncol]] = nnode
      graph[state.node] << [nnode, dist]
      neighbours.each do
        queue << State[node: nnode, nrow: _1, ncol: _2]
      end
      nodes << [state.node, state.nrow, state.ncol]

    end
    [graph, start, map[[forest.length - 1, forest[0].length - 2]]]
  end
end

pp Part1.main(ARGV[0])
pp Part2.main(ARGV[0])
