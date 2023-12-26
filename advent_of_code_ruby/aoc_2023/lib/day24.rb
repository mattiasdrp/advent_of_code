module Part1
  TestZone = Data.define(:rangex, :rangey) # => TestZone
  testzone = TestZone[rangex: (0..3), rangey: (0..4)] # => #<data TestZone rangex=0..3, rangey=0..4>

  module_function

  Point = Data.define(:x, :y) do
    def included?(testzone)
      testzone.rangex.include?(x) && testzone.rangey.include?(y)
    end
  end

  Hail = Data.define(:orig, :dx, :coef_x, :const) do
    def self.new_2D(x, y, dx, dy)
      coef_x = dy.to_f / dx
      const = y - coef_x * x
      orig = Point[x:, y:]
      self[orig:, coef_x:, dx:, const:]
    end

    def intersection(other)
      xi = (other.const - const).to_f / (coef_x - other.coef_x)
      yi = coef_x * xi + const
      pi = Point[x: xi, y: yi]
      time_self = (pi.x - orig.x).to_f / dx
      time_other = (pi.x - other.orig.x).to_f / other.dx
      [pi, time_self, time_other]
    end
  end

  def parse2D(line)
    coords, deltas = line.split(" @ ")
    cx, cy = coords.split(", ").map(&:to_i)
    dx, dy = deltas.split(", ").map(&:to_i)
    Hail.new_2D(cx, cy, dx, dy)
  end

  def main(file)
    min = (ARGV[1] || 200_000_000_000_000).to_i
    max = (ARGV[2] || 400_000_000_000_000).to_i
    testzone = TestZone[rangex: (min..max), rangey: (min..max)]
    File.readlines(file, chomp: true)
        .map { parse2D(_1) }
        .combination(2)
        .sum(0) do |h1, h2|
          point, time1, time2 = h1.intersection(h2)
          time1.positive? && time2.positive? && point.included?(testzone) ? 1 : 0
        end
  end
end

module Part2
  module_function

  Point = Data.define(:x, :y, :z)
  Hail = Data.define(:pos, :speed)

  def parse3D(line)
    coords, deltas = line.split(" @ ")
    pos = Point[*coords.split(", ").map(&:to_i)]
    speed = Point[*deltas.split(", ").map(&:to_i)]
    Hail[pos:, speed:]
  end

  # Let's say that rock is (pr, vr)
  # For each hail i, there exists a time ti such that
  # pr + vr*ti = pi + vi*ti
  # this can be rewritten as
  # pr - pi = -ti(vr - vi)
  # pr - pi and vr - vi are vectors, -ti is a scalar
  # this means that pr - pi and vr - vi have to be colinear
  # This means that their cross-product must be 0
  # (pr - pi) x (vr - vi) =
  #   (pr - pi)y.(vr - vi)z - (pr - pi)z.(vr - vi)y
  #   (pr - pi)z.(vr - vi)x - (pr - pi)x.(vr - vi)z
  #   (pr - pi)x.(vr - vi)y - (pr - pi)y.(vr - vi)x
  #
  # These 3 equations must be equal to 0
  #
  # let's see with (pr - pi)x.(vr - vi)y - (pr - pi)y.(vr - vi)x for i = 0 and i = 1
  #
  #       (A) prx.vrx - prx.v0y - vry.p0x + p0x.v0y - pry.vrx + pry.v0x + vrx.p0y - p0y.v0x = 0
  #
  #       (B) prx.vrx - prx.v1y - vry.p1x + p1x.v1y - pry.vrx + pry.v1x + vrx.p1y - p1y.v1x = 0
  #
  # (A) - (B) prx.(v1y - v0y) + pry.(v0x - v1x) + prz.0 + vrx.(p0y - p1y) + vry.(p1x - p0x) + vrz.0 = -p0x.v0y + p1x.v1y + p0y.v0x - p1y.v1x
  #
  # This is the first of the 6 needed equations (let's now replace as needed)
  #
  # (1) prx.0 + pry.(v1z - v0z) + prz.(v0y - v1y) + vrx.0 + vry.(p0z - p1z) + vrz.(p1y - p0y) = -p0y.v0z + p1y.v1z + p0z.v0y - p1z.v1y
  #
  # (2) prx.(v0z - v1z) + pry.0 + prz.(v1x - v0x) + vrx.(p1z - p0z) + vry.0 + vrz.(p0x - p1x) = -p0z.v0x + p1z.v1x + p0x.v0z - p1x.v1z
  #
  # (3) prx.(v1y - v0y) + pry.(v0x - v1x) + prz.0 + vrx.(p0y - p1y) + vry.(p1x - p0x) + vrz.0 = -p0x.v0y + p1x.v1y + p0y.v0x - p1y.v1x
  #
  # (4) prx.0 + pry.(v2z - v0z) + prz.(v0y - v2y) + vrx.0 + vry.(p0z - p2z) + vrz.(p2y - p0y) = -p0y.v0z + p2y.v2z + p0z.v0y - p2z.v2y
  #
  # (5) prx.(v0z - v2z) + pry.0 + prz.(v2x - v0x) + vrx.(p2z - p0z) + vry.0 + vrz.(p0x - p2x) = -p0z.v0x + p2z.v2x + p0x.v0z - p2x.v2z
  #
  # (6) prx.(v2y - v0y) + pry.(v0x - v2x) + prz.0 + vrx.(p0y - p2y) + vry.(p2x - p0x) + vrz.0 = -p0x.v0y + p2x.v2y + p0y.v0x - p2y.v2x
  #
  # We're doing this to solve Ax = B
  # A = 0           | (v1z - v0z) | (v0y - v1y) | 0           | (p0z - p1z) | (p1y - p0y)
  #     (v0z - v1z) | 0           | (v1x - v0x) | (p1z - p0z) | 0           | (p0x - p1x)
  #     (v1y - v0y) | (v0x - v1x) | 0           | (p0y - p1y) | (p1x - p0x) | 0
  #     0           | (v2z - v0z) | (v0y - v2y) | 0           | (p0z - p2z) | (p2y - p0y)
  #     (v0z - v2z) | 0           | (v2x - v0x) | (p2z - p0z) | 0           | (p0x - p2x)
  #     (v2y - v0y) | (v0x - v2x) | 0           | (p0y - p2y) | (p2x - p0x) | 0
  #
  # B = -p0y.v0z + p1y.v1z + p0z.v0y - p1z.v1y
  #     -p0z.v0x + p1z.v1x + p0x.v0z - p1x.v1z
  #     -p0x.v0y + p1x.v1y + p0y.v0x - p1y.v1x
  #     -p0y.v0z + p2y.v2z + p0z.v0y - p2z.v2y
  #     -p0z.v0x + p2z.v2x + p0x.v0z - p2x.v2z
  #     -p0x.v0y + p2x.v2y + p0y.v0x - p2y.v2x

  def matrix6(p0, p1, p2)
    v0 = p0.speed
    p0 = p0.pos
    v1 = p1.speed
    p1 = p1.pos
    v2 = p2.speed
    p2 = p2.pos

    a = [[0, (v1.z - v0.z), (v0.y - v1.y), 0, (p0.z - p1.z), (p1.y - p0.y)],
         [(v0.z - v1.z), 0, (v1.x - v0.x), (p1.z - p0.z), 0, (p0.x - p1.x)],
         [(v1.y - v0.y), (v0.x - v1.x), 0, (p0.y - p1.y), (p1.x - p0.x), 0],
         [0, (v2.z - v0.z), (v0.y - v2.y), 0, (p0.z - p2.z), (p2.y - p0.y)],
         [(v0.z - v2.z), 0, (v2.x - v0.x), (p2.z - p0.z), 0, (p0.x - p2.x)],
         [(v2.y - v0.y), (v0.x - v2.x), 0, (p0.y - p2.y), (p2.x - p0.x), 0]]
    b = [-p0.y * v0.z + p1.y * v1.z + p0.z * v0.y - p1.z * v1.y,
         -p0.z * v0.x + p1.z * v1.x + p0.x * v0.z - p1.x * v1.z,
         -p0.x * v0.y + p1.x * v1.y + p0.y * v0.x - p1.y * v1.x,
         -p0.y * v0.z + p2.y * v2.z + p0.z * v0.y - p2.z * v2.y,
         -p0.z * v0.x + p2.z * v2.x + p0.x * v0.z - p2.x * v2.z,
         -p0.x * v0.y + p2.x * v2.y + p0.y * v0.x - p2.y * v2.x]

    [a, b]
  end

  def main(file)
    points = File.readlines(file, chomp: true)
                 .map { parse3D(_1) }
    pp points[0]
    pp points[1]
    a, b = matrix6(points[0], points[1], points[2])
    # a.each_with_index.map do |line, index|
    #   equ =
    #     line.zip(%w[x y z a b c]).map do |coef, var|
    #       "#{coef} * #{var}"
    #     end.join(" + ")
    #   "#{equ} = #{b[index]}"
    # end.join("\n")
    a.each_with_index.map do |line, index|
      puts "#{line.join(" ")} #{b[index]}"
    end
  end
end

prx = 454_587_375_941_126
pry = 244_764_814_652_484
prz = 249_133_632_375_809

puts Part2.main(ARGV[0])
puts "#{[prx, pry, prz].sum}"
