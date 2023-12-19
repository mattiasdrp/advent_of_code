Rule = Data.define(:condition, :dest) do
  def pretty_print(pp)
    pp.text "condition "
    pp.pp condition
    pp.text " dest "
    pp.pp dest
  end
end

Workflow = Data.define(:rules)

module Part1
  module_function

  def main(file)
    s_workflows, s_parts = File.read(file).split("\n\n")
    workflows = {}
    s_workflows.split("\n").each { parse_workflow(_1, workflows) }
    s_parts.split("\n").sum(0) do |string|
      part = parse_part(string)
      apply_workflows(part, workflows, :in)
    end
  end

  def parse_rule(string)
    matching = /(?:(?<field>[xmas])(?<op>[><])(?<value>\d+):)?(?<dest>A|R|\w+)/.match(string)
    dest = matching[:dest].to_sym
    if matching[:field].nil?
      Rule[
        condition: ->(_) { true },
        dest:]
    else
      field = matching[:field].to_sym
      value = matching[:value].to_i
      Rule[
        condition: ->(part) { part[field].public_send(matching[:op], value) },
        dest:]
    end
  end

  def parse_workflow(line, workflows)
    matching = /(?<name>\w+){(?<rules>.*)}/.match(line)
    rules = matching[:rules].split(",")
    workflows[matching[:name].to_sym] = rules.map { parse_rule(_1) }
  end

  def parse_part(line)
    matching = /{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)}/.match(line)
    matching.named_captures.map { [_1.to_sym, _2.to_i] }.to_h
  end

  def apply_workflows(part, workflows, workflow)
    loop do
      res = workflows[workflow].find { _1.condition.call(part) }
      case res.dest
      in :R
        return 0
      in :A
        return part[:x] + part[:m] + part[:a] + part[:s]
      in dest
        workflow = dest
      end
    end
  end

  private :parse_workflow, :parse_rule, :parse_part, :apply_workflows
end

class Range
  def intersection(other)
    last_element = lambda { |range|
      range.exclude_end? ? range.max : range.last
    }
    self_max = last_element.call(self)
    other_max = last_element.call(other)
    return nil if other.first > self_max || first > other_max

    nfirst = [first, other.first].max
    nmax = [self_max, other_max].min
    diff =
      if first < nfirst
        first...nfirst
      else
        nmax + 1..self_max
      end
    [nfirst..nmax, diff]
  end

  alias & intersection
end

module Part2
  module_function

  def main(file)
    s_workflows, = File.read(file).split("\n\n")
    workflows = {}
    s_workflows.split("\n").each { parse_workflow(_1, workflows) }
    dfs workflows
  end

  Part = Data.define(:x, :m, :a, :s) do
    def pretty_print(pp)
      pp.text "x: #{x}, m: #{m}, a: #{a}, s: #{s}]"
    end
  end
  State = Data.define(:node, :part) do
    def pretty_print(pp)
      pp.text "node "
      pp.pp node
      pp.text " part "
      pp.pp part
    end
  end

  def update(part, rule)
    if rule.condition.nil?
      part.with
    else
      field = rule.condition[0]
      value = rule.condition[1]
      nfield, restfield = part.send(field) & value
      [part.with(field => nfield), part.with(field => restfield)]
    end
  end

  def dfs(workflows)
    stack = [State[node: :in, part: Part[x: (1..4000), m: (1..4000), a: (1..4000), s: (1..4000)]]]
    visited = {}
    total = 0
    until stack.empty?
      state = stack.pop
      next if visited.include?(state)

      visited[state] = true
      if state.node == :R
        next
      elsif state.node == :A
        total += state.part.x.size * state.part.m.size * state.part.a.size * state.part.s.size

      else
        workflows[state.node].reduce(state.part) do |part, rule|
          npart, restpart = update(part, rule)
          stack << State[node: rule.dest, part: npart]
          restpart
        end
      end
    end
    total
  end

  def parse_rule(string)
    matching = /(?:(?<field>[xmas])(?<op>[><])(?<value>\d+):)?(?<dest>A|R|\w+)/.match(string)
    dest = matching[:dest].to_sym
    if matching[:field].nil?
      Rule[
        condition: nil,
        dest:]
    else
      field = matching[:field].to_sym
      value = matching[:value].to_i
      range = case matching[:op]
              in "<"
                (1...value)
              in ">"
                (value + 1..4000)
              else
              end
      Rule[
        condition: [field, range],
        dest:]
    end
  end

  def parse_workflow(line, workflows)
    matching = /(?<name>\w+){(?<rules>.*)}/.match(line)
    rules = matching[:rules].split(",")
    workflows[matching[:name].to_sym] = rules.map { parse_rule(_1) }
  end

  private :parse_workflow, :parse_rule
end

pp(ARGV[1] == "1" ? Part1.main(ARGV[0]) : Part2.main(ARGV[0]))
