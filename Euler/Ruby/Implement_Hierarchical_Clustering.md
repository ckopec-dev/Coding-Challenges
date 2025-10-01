# Euler Problem - Hierarchical Clustering in Ruby

## Problem Statement
Implement hierarchical clustering algorithm to group similar data points together.

## Solution

```ruby
class Point
  attr_accessor :x, :y, :id
  
  def initialize(x, y, id = nil)
    @x = x
    @y = y
    @id = id || "#{x},#{y}"
  end
  
  def distance(other_point)
    Math.sqrt((@x - other_point.x)**2 + (@y - other_point.y)**2)
  end
  
  def to_s
    "(#{@x}, #{@y})"
  end
end

class Cluster
  attr_accessor :points, :id
  
  def initialize(points = [], id = nil)
    @points = points
    @id = id || "Cluster_#{Time.now.to_i}"
  end
  
  def centroid
    return nil if @points.empty?
    x_sum = @points.sum(&:x)
    y_sum = @points.sum(&:y)
    Point.new(x_sum / @points.length, y_sum / @points.length)
  end
  
  def merge(other_cluster)
    new_points = @points + other_cluster.points
    Cluster.new(new_points, "#{@id}_merged_#{other_cluster.id}")
  end
  
  def size
    @points.length
  end
  
  def to_s
    "Cluster #{@id}: #{@points.length} points"
  end
end

class HierarchicalClustering
  def initialize(points)
    @points = points
    @clusters = points.map { |point| Cluster.new([point]) }
  end
  
  def cluster(max_clusters = 1)
    # Keep merging until we reach the desired number of clusters
    while @clusters.length > max_clusters
      closest_pair = find_closest_clusters
      break unless closest_pair
      
      cluster1, cluster2 = closest_pair
      merged_cluster = cluster1.merge(cluster2)
      
      # Remove the two old clusters and add the new one
      @clusters.delete(cluster1)
      @clusters.delete(cluster2)
      @clusters << merged_cluster
    end
    
    @clusters
  end
  
  def find_closest_clusters
    min_distance = Float::INFINITY
    closest_pair = nil
    
    @clusters.each_with_index do |cluster1, i|
      @clusters[i+1..-1].each do |cluster2|
        distance = cluster_distance(cluster1, cluster2)
        if distance < min_distance
          min_distance = distance
          closest_pair = [cluster1, cluster2]
        end
      end
    end
    
    closest_pair
  end
  
  def cluster_distance(cluster1, cluster2)
    # Using single linkage (minimum distance between points)
    min_distance = Float::INFINITY
    
    cluster1.points.each do |point1|
      cluster2.points.each do |point2|
        distance = point1.distance(point2)
        min_distance = distance if distance < min_distance
      end
    end
    
    min_distance
  end
  
  def print_clusters(clusters)
    clusters.each_with_index do |cluster, index|
      puts "Cluster #{index + 1}:"
      cluster.points.each { |point| puts "  #{point}" }
      puts "Centroid: #{cluster.centroid}"
      puts ""
    end
  end
end

# Example usage
def euler_problem_250_example
  # Create sample points
  points = [
    Point.new(1, 2),
    Point.new(1, 4),
    Point.new(1, 0),
    Point.new(10, 2),
    Point.new(10, 4),
    Point.new(10, 0)
  ]
  
  puts "Original Points:"
  points.each { |point| puts point }
  puts "\n"
  
  # Perform hierarchical clustering
  hc = HierarchicalClustering.new(points)
  clusters = hc.cluster(2)
  
  puts "Final Clusters:"
  hc.print_clusters(clusters)
  
  return clusters
end

# More complex example with random data
def generate_random_points(n = 10)
  points = []
  n.times do
    x = rand(0..100)
    y = rand(0..100)
    points << Point.new(x, y)
  end
  points
end

def euler_problem_250_advanced_example
  puts "=== Advanced Hierarchical Clustering Example ==="
  
  # Generate random points
  points = generate_random_points(15)
  
  puts "Generated Points:"
  points.each_with_index { |point, i| puts "Point #{i + 1}: #{point}" }
  puts "\n"
  
  # Perform clustering into 3 groups
  hc = HierarchicalClustering.new(points)
  clusters = hc.cluster(3)
  
  puts "Final Clusters (3 groups):"
  hc.print_clusters(clusters)
  
  # Show cluster statistics
  puts "Cluster Statistics:"
  clusters.each_with_index do |cluster, index|
    centroid = cluster.centroid
    puts "Cluster #{index + 1}: #{cluster.size} points, Centroid: #{centroid}"
  end
  
  return clusters
end

# Run examples
puts "=== Euler Problem - Hierarchical Clustering ==="
puts

euler_problem_250_example
puts "=" * 50
puts
euler_problem_250_advanced_example

# Additional utility methods for Euler problems
class HierarchicalClustering
  # Calculate within-cluster sum of squares
  def wcss(clusters)
    total_wcss = 0
    clusters.each do |cluster|
      centroid = cluster.centroid
      cluster_wcss = cluster.points.sum do |point|
        point.distance(centroid) ** 2
      end
      total_wcss += cluster_wcss
    end
    total_wcss
  end
  
  # Calculate between-cluster sum of squares
  def bcss(clusters, all_points)
    total_centroid = Cluster.new(all_points).centroid
    total_bcss = 0
    
    clusters.each do |cluster|
      centroid = cluster.centroid
      distance_to_total = total_centroid.distance(centroid)
      total_bcss += cluster.size * (distance_to_total ** 2)
    end
    
    total_bcss
  end
  
  # Calculate silhouette score (simple version)
  def silhouette_score(clusters, all_points)
    return 0 if clusters.length <= 1
    
    total_score = 0
    all_points.each do |point|
      cluster_index = clusters.find_index { |cluster| cluster.points.include?(point) }
      cluster = clusters[cluster_index]
      
      # Calculate average distance to points in same cluster (a)
      a = cluster.points.reject { |p| p == point }.map { |p| point.distance(p) }.sum / [1, cluster.points.length - 1].max
      
      # Calculate minimum average distance to points in other clusters (b)
      b = Float::INFINITY
      clusters.each_with_index do |other_cluster, i|
        next if i == cluster_index
        avg_distance = other_cluster.points.map { |p| point.distance(p) }.sum / other_cluster.points.length
        b = [b, avg_distance].min
      end
      
      # Silhouette score for this point
      s = (b - a) / [a, b].max
      total_score += s
    end
    
    total_score / all_points.length
  end
end

# Demonstrate additional metrics
puts "\n=== Additional Metrics ==="
points = generate_random_points(10)
hc = HierarchicalClustering.new(points)
clusters = hc.cluster(2)

puts "WCSS: #{hc.wcss(clusters)}"
puts "BCSS: #{hc.bcss(clusters, points)}"
puts "Silhouette Score: #{hc.silhouette_score(clusters, points).round(4)}"
```

## Key Features

1. **Point Class**: Represents 2D coordinates with distance calculation
2. **Cluster Class**: Groups points and calculates centroids
3. **HierarchicalClustering**: Main clustering algorithm implementation
4. **Single Linkage**: Uses minimum distance between clusters
5. **Flexible Output**: Supports different numbers of clusters

## Time Complexity
- O(n²) for finding closest clusters in each iteration
- Overall: O(n³) where n is the number of points

## Space Complexity
- O(n²) for storing distances between all point pairs

## Usage Examples
The code demonstrates:
1. Basic clustering with predefined points
2. Random data generation and clustering
3. Cluster statistics and evaluation metrics
4. Multiple cluster configurations

This implementation provides a solid foundation for solving Euler problems involving data clustering and grouping.

