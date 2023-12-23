# frozen_string_literal: true

Brick = Struct.new(:name, :cubes, :type, :height) do
  def pretty_print(pp)
    pp.pp name
  end

  def to_s
    "{ name: #{name}, cubes: #{cubes}}"
  end
end

def fill(map, bricks, line, index)
  name = index.to_s
  start_pos, end_pos = line.split("~")
  sx, sy, sz = start_pos.split(",").map(&:to_i)
  ex, ey, ez = end_pos.split(",").map(&:to_i)
  cubes = []
  (sx..ex).each do |x|
    (sy..ey).each do |y|
      (sz..ez).each do |z|
        cubes << [x, y, z]
      end
    end
  end
  type = sz == ez ? :horizontal : :vertical
  brick = Brick.new(name:, cubes:, type:, height: sz)
  cubes.each { map[_1] = brick }
  bricks << brick
  [ex, ey, ez]
end

def max(val1, val2)
  val1 > val2 ? val1 : val2
end

def find_ground(grid3d, x, y, z)
  (z - 1).downto(1).each do |zp|
    return [zp + 1, grid3d[zp][x][y]] unless grid3d[zp][x][y] == "."
  end
  nil
end

def fall(grid3d, bricks)
  bricks.each do |brick|
    cubes = brick.type == :vertical ? [brick.cubes[0]] : brick.cubes
    zp = cubes.reduce(1) do |zp, (x, y, z)|
      case find_ground(grid3d, x, y, z)
      in nil
        zp
      in [zpp, brick_stop]
        zpp > zp ? zpp : zp
      else
      end
    end
    next unless zp < brick.height

    case brick.type
    in :horizontal
      brick.cubes.map! do |(x, y, z)|
        grid3d[zp][x][y] = brick
        grid3d[z][x][y] = "."
        [x, y, zp]
      end
    in :vertical
      brick.cubes.map!.each_with_index do |(x, y, z), i|
        grid3d[zp + i][x][y] = brick
        grid3d[z][x][y] = "."
        [x, y, zp + i]
      end
    else
    end
  end
end

def parse(file)
  map = {}
  bricks = []
  mx, my, mz = File.readlines(file, chomp: true)
                   .each_with_index
                   .reduce([0, 0, 0]) do |(mx, my, mz), (line, index)|
    x, y, z = fill(map, bricks, line, index)
    [max(x, mx), max(y, my), max(z, mz)]
  end
  grid3d =
    Array.new(mz + 1) do |z|
      Array.new(mx + 1) do |x|
        Array.new(my + 1) do |y|
          case map[[x, y, z]]
          in nil
            "."
          in v
            v
          else
          end
        end
      end
    end
  bricks.sort_by!(&:height)
  fall(grid3d, bricks)
  map = {}
  inv_map = {}
  bricks.each do |brick|
    brick.cubes.each do |(x, y, z)|
      next unless z < grid3d.length - 1

      case grid3d[z + 1][x][y]
      in "."
        next
      in b if b.name != brick.name
        map[brick].nil? ? map[brick] = Set[b] : map[brick].add(b)
        inv_map[b].nil? ? inv_map[b] = Set[brick] : inv_map[b].add(brick)
      else
      end
    end
  end
  [bricks, map, inv_map]
end

def part1(file)
  bricks, map, inv_map = parse file
  bricks.sum do |brick|
    supported = map[brick]
    supported.nil? || supported.all? { |b| inv_map[b].length > 1 } ? 1 : 0
  end
end

def disintegrate(brick, map, inv_map, supported)
  queue = [[brick, supported]]
  until queue.empty?
    brick, supported = queue.pop
    next if supported.nil?

    supported.each do |b|
      inv_map[b].delete(brick)
      # print "adding #{b}: "
      # pp map[b]
      queue << [b, map[b]] if inv_map[b].empty?
    end
  end
  inv_map.sum(0) { |_, set| set.empty? ? 1 : 0 }
end

class Hash
  def deep_copy
    ser_map = Marshal.dump(self)
    Marshal.load(ser_map)
  end
end

# Needs to be optimised
def part2(file)
  _, map, inv_map = parse file
  map.sum(0) do
    tmp_inv = inv_map.deep_copy
    disintegrate(_1, map, tmp_inv, _2)
  end
end

# puts part1(ARGV[0])
puts part2(ARGV[0])
