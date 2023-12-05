# frozen_string_literal: true

# dict is [:dest, :ranges]
def parse(result, line)
  matching = /seeds: ([\d ]+)/.match(line)
  return if line.empty?

  if matching.nil?
    matching = /([[:alpha:]]+)-to-([[:alpha:]]+) map:/.match(line)
    if matching.nil?
      # The line is a mapping
      ndest, nsource, range = line.split(/[[:blank:]]/).map(&:to_i)
      content = result[:dict][result[:source]][:ranges]
      result[:dict][result[:source]][:ranges] =
        content.add({ ndest:, nsource:, range: })
    else
      # the line is 'a-to-b map:'
      result[:source] = matching[1]
      result[:dest] = matching[2]
      result[:dict][matching[1]] = { dest: matching[2], ranges: Set[] }
    end
  else
    # the line is the seeds
    result[:seeds] = matching[1].split(/[[:blank:]]/).map(&:to_i)
  end
end

def max(val1, val2)
  val1 > val2 ? val1 : val2
end

def min(val1, val2)
  val1 < val2 ? val1 : val2
end

def traverse1(curr_value, dict, source)
  if dict[source].nil?
    curr_value
  else
    result = dict[source][:ranges].find do |range|
      range[:nsource] <= curr_value && curr_value < range[:nsource] + range[:range]
    end
    new_value =
      if result.nil?
        curr_value
      else
        result => {nsource:, ndest:}
        ndest + curr_value - nsource
      end
    traverse1(new_value, dict, dict[source][:dest])
  end
end

def traverse2(seed_start, seed_end, min_value, dict, source, slices)
  if dict[source].nil?
    min_value = min(min_value, seed_start)
    if slices.empty?
      min_value
    else
      new_start, new_end, new_source = slices.shift
      traverse2(new_start, new_end, min_value, dict, new_source, slices)
    end
  else
    result = dict[source][:ranges].find do |range|
      # Find if the current value with its range has an intersection
      # with the range

      # First case, the seed_start is between the bounds
      (range[:nsource] <= seed_start && seed_start < range[:nsource] + range[:range]) \
        || (range[:nsource] <= seed_start && seed_start < range[:nsource] + range[:range] && range[:nsource] < seed_end)
    end
    new_start, new_end =
      if result.nil?
        # The current interval [seed_start..seed_end]
        # doesn't fit any range
        [seed_start, seed_end]
      else
        result => {nsource:, ndest:, range:}
        slices.push([seed_start, nsource, source]) if seed_start < nsource
        slices.push([nsource + range, seed_end, source]) if nsource + range < seed_end
        if seed_start <= nsource
          [ndest, ndest + min(range, seed_end - nsource)]
        else
          offset = seed_start - nsource
          [ndest + offset, ndest + offset + min(range - offset, seed_end - seed_start)]
        end
      end
    traverse2(new_start, new_end, min_value, dict, dict[source][:dest], slices)
  end
end

def parse_file(file, part)
  result = { dict: {}, seeds: [], source: "", dest: "" }
  File.readlines(file, chomp: true).each do |line|
    parse(result, line)
  end
  total = 2**64
  if part == 1
    result[:seeds].each do |seed|
      dest = traverse1(seed, result[:dict], "seed")
      total = min(total, dest)
    end
  else
    result[:seeds].each_slice(2) do |seed, range|
      total = min(total, traverse2(seed, seed + range, total, result[:dict], "seed", []))
    end
  end
  total
end

puts parse_file(ARGV[0], ARGV[1].to_i)
