def random_edge(graph)
  key = graph.keys.sample
  [key, graph[key].sample]
end

def karger(graph)
  while graph.length > 2
    node1, node2 = random_edge(graph)
    edges1 = graph.delete(node1)
    edges2 = graph.delete(node2)
    edges = edges1 + edges2 - [node1, node2]
    nnode = node1 + node2
    graph[nnode] = edges
    graph.each_value do |array|
      array.map! { |x| x == node1 || x == node2 ? nnode : x }
    end
  end
  graph
end

def find_cut(file)
  graph = Hash.new { |h, k| h[k] = [] }
  File.readlines(file, chomp: true).each do |line|
    matching = /(?<comp>\w+): (?<rest>[\w ]+)/.match(line)
    others = matching[:rest].split(" ")
    others.each do |comp2|
      graph[[matching[:comp].to_sym]] << [comp2.to_sym]
      graph[[comp2.to_sym]] << [matching[:comp].to_sym]
    end
  end
  karger(graph)
end

def part1(file)
  res = loop do
    res = find_cut(file)
    break res if res.all? { |_, value| value.length == 3 }
  end
  res.reduce(1) { |res, (key, _)| res * key.length }
end

pp part1(ARGV[0])
