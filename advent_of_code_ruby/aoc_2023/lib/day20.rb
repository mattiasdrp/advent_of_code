Mod = Data.define(:type, :content, :dest) do
  def pretty_print(pp)
    pp.pp type
    pp.text " "
    pp.pp content
    pp.text " "
    pp.pp dest
  end
end

def parse(line, map)
  matchings = /(%|&)?(\w+) -> ([\w, ]+)/.match(line)
  type, content =
    case matchings[1]
    in nil
      [:broadcaster, nil]
    in "%"
      [:flipflop, false]
    in "&"
      [:conjunction, {}]
    else
    end
  dest = matchings[3].split(", ").map(&:to_sym)
  map[matchings[2].to_sym] = Mod[type:, content:, dest:]
end

State = Data.define(:node, :from, :beam)

def press_button(map, target = nil)
  queue = [State[node: :broadcaster, from: nil, beam: :low]]
  high_beams = 0
  low_beams = 1
  until queue.empty?
    state = queue.shift
    node = map[state.node]
    next if node.nil?

    beam = state.beam
    if node.type == :flipflop
      next if beam == :high

      map[state.node] = node.with(content: !node.content)
      beam = if node.content
               :low
             else
               :high
             end
    elsif node.type == :conjunction
      node.content[state.from] = beam
      # beam = node.content.all? { |_, value| value == :high } ? :low : :high
      beam =
        if node.content.all? { |_, value| value == :high }
          return nil if target == state.node

          :low
        else

          :high
        end
    end

    map[state.node].dest.each do |nnode|
      if beam == :high
        high_beams += 1
      else
        low_beams += 1
      end
      # puts "#{state.node} -#{beam}-> #{nnode}"
      queue << State[node: nnode, from: state.node, beam:]
    end
  end
  [high_beams, low_beams]
end

def loop(map)
  high_beams = 0
  low_beams = 0
  1000.times do
    hb, lb = press_button(map)
    high_beams += hb
    low_beams += lb
  end
  [high_beams, low_beams]
end

def parse_map(file)
  map = {}
  File.readlines(file, chomp: true).each { parse(_1, map) }
  map.each do |key, value|
    next unless value.type == :conjunction

    map.each do |keyp, valuep|
      value.content[keyp] = :low if valuep.dest.include?(key)
    end
  end
  map
end

def part1(file)
  map = parse_map(file)
  # initial = map.clone
  high_beams, low_beams = loop(map)
  high_beams * low_beams
end

def loop2(map, target)
  (1..).each do |step|
    return step if press_button(map, target).nil?
  end
end

def part2(file)
  map1 = parse_map(file)
  map2 = parse_map(file)
  map3 = parse_map(file)
  map4 = parse_map(file)
  step_qz = loop2(map1, :zq)
  step_cq = loop2(map2, :kx)
  step_jx = loop2(map3, :zd)
  step_tt = loop2(map4, :mt)
  [step_cq, step_jx, step_qz, step_tt].reduce(&:lcm)
end

pp(ARGV[1] == "1" ? part1(ARGV[0]) : part2(ARGV[0]))
