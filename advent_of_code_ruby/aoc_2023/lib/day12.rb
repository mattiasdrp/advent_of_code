# frozen_string_literal: true

class Solver
  def initialize
    @memo = {}
  end

  def combinations(line, part)
    line, groups = line.split
    groups = groups.split(",").map(&:to_i)
    if part == 1
      nb_combinations(line, groups)
    else
      line = ([line] * 5).join("?")
      nb_combinations(line, groups * 5)
    end
  end

  private

  def nb_combinations_aux(line, line_index, group, groups, next_group_index, tmp_line)
    # puts "line #{line[line_index..]}"
    # puts "  group #{group}"
    # puts "  groups #{groups[next_group_index - 1..]} "
    # puts "  tmp_line #{tmp_line.join}"

    # If we consumed too much or not enough # we exit early
    return 0 if !group.nil? && group.negative?

    case line[line_index]
    in "#"
      if !group.nil?
        # If we see # and are consuming a group, keep consuming
        nb_combinations_aux(line, line_index + 1, group - 1, groups, next_group_index, tmp_line + ["#"])
      else
        # We were not in a group
        # If we don't have any more groups, end this branch
        return 0 if next_group_index >= groups.length

        # Otherwise, let's start consuming the next one
        group = groups[next_group_index]
        nb_combinations_aux(line, line_index + 1, group - 1, groups, next_group_index + 1, tmp_line + ["#"])
      end
    in "."
      # It should be noted that . (and ? as .) is the only character that can
      # nilify a group meaning that when group is nil we must have consumed a .
      if !group.nil?
        # If we see . and are consuming a group
        if group.positive?
          # If we didn't finish the current group, end this branch
          0
        else
          nb_combinations_aux(line, line_index + 1, nil, groups, next_group_index, tmp_line + ["."])
        end
      else
        # If we see . and are not consuming a group
        nb_combinations_aux(line, line_index + 1, nil, groups, next_group_index, tmp_line + ["."])
      end
    in "?"
      if !group.nil? && group.positive?
        # We are consuming a group and did not finish it, ? can't be anything else than #
        nb_combinations_aux(line, line_index + 1, group - 1, groups, next_group_index, tmp_line + ["#"])
      elsif !group.nil? && group.zero?
        # We were consuming a group and finished it, ? can't be anything else than .
        nb_combinations_aux(line, line_index + 1, nil, groups, next_group_index, tmp_line + ["."])
      elsif group.nil?
        memo_key = [line[line_index..], group, groups[next_group_index..]]
        memo_value = @memo[memo_key]
        return memo_value unless memo_value.nil?

        # We are not consuming a group, here we create 2 branches:
        # - one where we treat ? as # if we have more groups to consume
        q_as_sharp =
          if next_group_index >= groups.length
            0
          else
            group = groups[next_group_index]
            nb_combinations_aux(line, line_index + 1, group - 1, groups, next_group_index + 1, tmp_line + ["#"])
          end
        # - one where we treat ? as .
        q_as_dot = nb_combinations_aux(line, line_index + 1, nil, groups, next_group_index, tmp_line + ["."])
        res = q_as_dot + q_as_sharp
        @memo[memo_key] = res
        res
      end
    else
      if next_group_index == groups.length && (group.nil? || group.zero?)
        # puts "  --- FOUND #{tmp_line.join} ---"
        # puts "    for #{line} #{groups} ---"
        1
      else
        0
      end
    end
  end

  def nb_combinations(line, groups)
    nb_combinations_aux(line, 0, nil, groups, 0, [])
  end
end

def part(file, part)
  res = 0
  File.readlines(file, chomp: true).each do
    solver = Solver.new
    res += solver.combinations(_1, part)
  end
  res
end

puts(ARGV[1] == "1" ? part(ARGV[0], 1) : part(ARGV[0], 2))
