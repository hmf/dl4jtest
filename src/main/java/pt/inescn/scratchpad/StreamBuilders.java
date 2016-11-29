package pt.inescn.scratchpad;

/**
 * 
 */

import java.io.IOException;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.distribution.ParetoDistribution;
import org.apache.commons.math3.distribution.UniformIntegerDistribution;
import org.apache.commons.math3.distribution.WeibullDistribution;
import org.apache.commons.math3.random.JDKRandomGenerator;
import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.util.Pair;
import org.knowm.xchart.CategoryChart;
import org.knowm.xchart.CategoryChartBuilder;
import org.knowm.xchart.Histogram;
import org.knowm.xchart.SwingWrapper;
import org.knowm.xchart.VectorGraphicsEncoder;
import org.knowm.xchart.VectorGraphicsEncoder.VectorGraphicsFormat;
import org.knowm.xchart.style.Styler.LegendPosition;

/**
 * @author hmf
 *
 *  sbt "run-main pt.inescn.scratchpad.StreamBuilders"
 *  @see https://github.com/marcellodesales/jdk8-features-examples/blob/master/src/main/java/org/github/marcellodesales/streams/StreamImplementation.java
 */
public class StreamBuilders {
  
  public static <T> void printArray(T[] v){
    System.out.print("{");
    Arrays.stream(v).forEach(e -> { System.out.print(e); System.out.print(","); } );
    System.out.println("}");
  }

  /**
   * Allows the zipping of two streams. No parallel access allowed.
   * 
   * @param as
   * @param bs
   * @return
   */
  static <A, B> Stream<Pair<A, B>> zip(Stream<A> as, Stream<B> bs) {
    Iterator<A> i1 = as.iterator();
    Iterator<B> i2 = bs.iterator();
    Iterable<Pair<A, B>> i = () -> new Iterator<Pair<A, B>>() {
      public boolean hasNext() {
        return i1.hasNext() && i2.hasNext();
      }

      public Pair<A, B> next() {
        return new Pair<A, B>(i1.next(), i2.next());
      }
    };
    return StreamSupport.stream(i.spliterator(), false);
  }

  /**
   * Creates a stream of integers. Starts at n (inclusive). No limit set. Use
   * the <code>limit</code> method for that.
   * 
   * @param n
   * @return
   */
  static Stream<Integer> StreamOfInts(AtomicInteger n) {
    return Stream.generate(() -> n.incrementAndGet());
  }
  
  /**
   * Creates a stream on the integer n.
   * 
   * @param n
   * @return
   */
  static Stream<Integer> StreamOfInt(Integer n) {
    return Stream.generate(() -> n);
  }

  /**
   * Takes a stream of pairs and generates a stream a new stream where the label
   * (first element of the pair) is repeated as many times as indicated in the
   * second pair element.
   * 
   * @param pairs
   * @return
   */
  static Stream<Integer> rep(Stream<Pair<Integer, Integer>> pairs) {
    Stream<Integer> r = pairs.flatMap(p -> {
      Stream<Integer> i = StreamOfInt( p.getFirst() ).limit(p.getSecond());
      return i;
    });

    return r;
  }

  /**
   * 
   * @param r
   * @param randomNumberOrigin
   *          the origin (inclusive) of each random value
   * @param randomNumberBound
   *          the bound (exclusive) of each random value
   * @return
   */
  public static IntStream uniformInts(Random r, int randomNumberOrigin, int randomNumberBound) {
    return r.ints(randomNumberOrigin, randomNumberBound);
  }

  
  /**
   * Creates a stream of random longs (uniform distribution). 
   * 
   * @param r
   * @return
   */
  public static LongStream uniformLongs(Random r) {
    return r.longs();
  }

  /**
   * Crates a stream of random real values (uniform distribution). 
   * 
   * @param r
   * @return
   */
  public static DoubleStream uniformDoubles(Random r) {
    return r.doubles();
  }

  /**
   * Creates a set of (integer) labels (from 0 to the length of <code>dist</code>). 
   * The labels are generated according to the distribution indicated in <code>dist</code>.
   * The values of <code>dist</code> should sum to 100 (%). each abel is generated
   * with the <code>dist</code> distribution. 
   * 
   * @param r
   * @param dist
   * @return
   */
  public static Stream<Integer> discreteDistInts(RandomGenerator r, List<Integer> dist) {

    // Convert integer distributions into a stream and check that they add up to
    // 100% 
    Stream<Integer> s = dist.stream();
    int sm = s.mapToInt(i -> i.intValue()).sum();
    if (sm != 100) {
      throw new IllegalArgumentException("Integers represent percentage so must add up to 100.");
    }

    // Create a stream on integer labels
    // IntStream labels = IntStream.range(0, dist.size());
    AtomicInteger n = new AtomicInteger(0);
    Stream<Integer> labels = StreamOfInts(n);

    // Pair each label with the percentage we want
    Stream<Pair<Integer, Integer>> pairs = zip( labels, dist.stream());
    // Duplicate each label as many times as indicated in the distribution
    Stream<Integer> dist_labels = rep(pairs);

    // These are the label distributions (repeated according to dist)
    Integer[] labels_idx = dist_labels.toArray(Integer[]::new);
    
    // Sample these uniformly
    UniformIntegerDistribution nd = new UniformIntegerDistribution(r, 0, labels_idx.length - 1);
    Stream<Integer> stream = Stream.generate(() -> labels_idx[ nd.sample() ] );
    
    return stream;
  }

  /**
   * Creates a stream of time-stamps (sampling as fast as the machine allows)
   * 
   * @param r
   * @return
   */
  public static Stream<ZonedDateTime> samplingZonedDateTime(LocalDateTime start, Duration samplingRate) {
    ZoneId currentZone = ZoneId.systemDefault();
    // ZoneId lisbon = ZoneId.of("Europe/Lisbon");
    
    //LocalDateTime now = LocalDateTime.now();
    ZonedDateTime startTime = ZonedDateTime.of(start, currentZone);
    // sampling rate converted to delta durations
    Stream<Duration> sampling = Stream.iterate(samplingRate, s -> s.plus(samplingRate));
    // Add delta to start the to get final sampled date time-stamp
    return sampling.map( s -> startTime.plus( s ));
  }

  public static Stream<ZonedDateTime> samplingZonedDateTime(LocalDateTime start, Duration samplingRate, ZoneId zone) {
    // ZoneId lisbon = ZoneId.of("Europe/Lisbon");
    
    //LocalDateTime now = LocalDateTime.now();
    ZonedDateTime startTime = ZonedDateTime.of(start, zone);
    // sampling rate converted to delta durations
    Stream<Duration> sampling = Stream.iterate(samplingRate, s -> s.plus(samplingRate));
    // Add delta to start the to get final sampled date time-stamp
    return sampling.map( s -> startTime.plus( s ));
  }
  
  public static Stream<Double> normalDist(RandomGenerator r, double mean, double sd){
    NormalDistribution d = new NormalDistribution(r, mean, sd);
    return Stream.generate(() -> d.sample() );
  }
  
  public static Stream<Double> weibullDist(RandomGenerator r, double alpha, double beta){
    WeibullDistribution d = new WeibullDistribution(r, alpha, beta);
    return Stream.generate(() -> d.sample() );
  }
  
  public static Stream<Double> paretoDist(RandomGenerator r, double alpha, double beta){
    ParetoDistribution d = new ParetoDistribution(r, alpha, beta);
    return Stream.generate(() -> d.sample() );
  }

  
  public static Integer[] toDiscreteWeights(Double scale, Double[] weights) {
    boolean are_fractions = Arrays.stream(weights).allMatch( e -> (e < 1.0) && (e > 0.0));
    if (!are_fractions)
      throw new IllegalArgumentException("All weights must be fractions");
    
    Double total = Arrays.stream(weights).reduce(0.0, (acc,b) -> acc + b);
    if (total != 1.0)
      throw new IllegalArgumentException("All weights must sum to 1.0: " +  total.toString());
      
    // Get the min fraction and convert all floats to a integer by the inverse of this min 
    Double min = Arrays.stream(weights).map(e -> Math.abs(e) ).reduce(Double.MAX_VALUE, (acc,b) -> { if (b < acc) acc = b; return acc; } );
    // Make sure the least weight is representative enough for sampling
    Double max = scale * (1.0 / min);
    List<Double> r = Arrays.stream(weights).map(e -> e * max ).collect(Collectors.toList());
    
    // Check that resulting re-scaled reals are in fact integers 
    Stream<Double> r1 = r.stream().map(e -> e - e.intValue());
    boolean are_rational_fractions = r1.allMatch( e -> e == 0.0);
    if (!are_rational_fractions)
      throw new IllegalArgumentException("All weights must be rational fractions");
    
    // Return the integer weight
    Stream<Integer> r2 = r.stream().map(e -> e.intValue());
    //return r2.collect( Collectors.toList());
    return r2.toArray(Integer[]::new);
  }
  
  public static Integer[] toDiscreteDist(Integer[] weights) {

    // Create a stream on integer labels
    // IntStream labels = IntStream.range(0, dist.size());
    AtomicInteger n = new AtomicInteger(0);
    Stream<Integer> labels = StreamOfInts(n);

    // Pair each label with the percentage we want
    Stream<Pair<Integer, Integer>> pairs = zip( labels, Arrays.stream(weights));
    // Duplicate each label as many times as indicated in the distribution
    Stream<Integer> dist_labels = rep(pairs);

    // These are the label distributions (repeated according to dist)
    Integer[] labels_idx = dist_labels.toArray(Integer[]::new);
    
    return labels_idx;
  }
  
  public static Stream<Integer> genericSample(Supplier<Integer> sampler, Integer[] labels_idx) {
    
    // Sample these weighted labels 
    Stream<Integer> stream = Stream.generate(() -> labels_idx[ sampler.get() ] );
    
    // Samples returned as a stream
    return stream;
  }
  
  public static Stream<Integer> uniformSampler(Integer[] weights) {
    
    // Generate an array of labels according to the weights distribution
    Integer[] labels_idx = toDiscreteDist(weights);
    
    // Sample these uniformly
    UniformIntegerDistribution nd = new UniformIntegerDistribution(0, labels_idx.length - 1);
    Supplier<Integer> sampler = () -> nd.sample();
    return genericSample(sampler, labels_idx);
  }
  
  
  public static Stream<Double> mixtureModel(RandomGenerator r, double scale, Double[] weights, ArrayList<Stream<Double>> ts) {
    
    // Generate an equivalent weights distribution as integers (scale them)
    Integer[] dweights = toDiscreteWeights(scale, weights);
    
    // Sample these uniformly to get distribution of indexes
    Stream<Integer> select_stream = uniformSampler(dweights);
    //select_stream.limit(10).forEach(System.out::println);
    
    Stream<Iterator<Double>> tss = ts.stream().map(e -> e.iterator());
    //Iterator<Double> identity;
    // new ArrayList<Iterator<Double>>[2], 
    //tss.reduce(identity, (acc,l) -> acc.add(l));
    ArrayList<Iterator<Double>> l = tss.collect(Collectors.toCollection(ArrayList::new));;
    
    // Use the indexes to get the selected distribution    t = ts.get(0) ;
    //Stream<Stream<Double>> dist_stream = select_stream.map( idx -> ts[ idx ] );
    //Stream<Iterator<Double>> dist_stream = select_stream.map( idx -> ts.get( idx ).iterator() );
    Stream<Iterator<Double>> dist_stream = select_stream.map( idx -> {
      //System.out.println("ts.length = " +  ts.size());
      //System.out.println("idx = " +  (idx - 1));
      //return ts.get( idx - 1 ).iterator();
      return l.get(idx - 1);
    } );
    
    // Create a stream of the selected distribution's output
    Stream<Double> stream = dist_stream.map( it -> it.next());
    
    return stream;
  }  
  
  public static Stream<Double> biModalDist(RandomGenerator r, double scale, Double[] weights, double mean_1, double sd_1, double mean_2, double sd_2) {
    // Java sucks!
    // http://stackoverflow.com/questions/2927391/whats-the-reason-i-cant-create-generic-array-types-in-java
    //Stream<Double>[] ts = (Stream<Double>[]) new Object[]{ normalDist(r, mean_1, sd_1), normalDist(r, mean_2, sd_2)};
    List<Stream<Double>>       l = Arrays.asList(normalDist(r, mean_1, sd_1), normalDist(r, mean_2, sd_2));
    ArrayList<Stream<Double>> ts = new ArrayList<Stream<Double>>( l );
    
    return mixtureModel(r, scale, weights, ts);
  }  
  
  /*public static Stream<Double> dataSetColumn() {
    
  }*/
  
  
  public static void main(String[] args) {
    AtomicInteger n = new AtomicInteger(0);
    Stream<Integer> stream1 = Stream.generate(() -> n.incrementAndGet());
    stream1.limit(10).limit(100).forEach(p -> System.out.println(p));

    //Stream<Date> stream2 = Stream.generate(() -> new Date() );
    Duration sampling1 = Duration.ofMillis(30);
    Stream<ZonedDateTime> stream2 = samplingZonedDateTime( LocalDateTime.now(), sampling1);
    stream2.limit(10).forEach(p -> System.out.println(p));

    Duration sampling2 = Duration.ofSeconds(1);
    Stream<ZonedDateTime> stream3 = samplingZonedDateTime( LocalDateTime.now(), sampling2,  ZoneId.of("Europe/Berlin") );
    stream3.limit(10).forEach(p -> System.out.println(p));

    JDKRandomGenerator r = new JDKRandomGenerator() ;
    List<Integer> dist = Arrays.asList(50, 30, 20);
    Stream<Integer> stream4 = discreteDistInts(r, dist);
    //stream4.limit(10).forEach(p -> System.out.println(p));
    System.out.println( stream4.limit(1000).filter(e -> e == 2).count() );
    
    Stream<Double> stream5 = normalDist(r, 1000.0, 10.0);
    stream5.limit(10).forEach(p -> System.out.println(p));
    
    Stream<Double> stream6 = weibullDist(r, 100.0, 3.0);
    stream6.limit(10).forEach(p -> System.out.println(p));
    
    Stream<Double> stream7 = paretoDist(r, 10.0, 10.0);
    stream7.limit(10).forEach(p -> System.out.println(p));
    
    Integer[] weights1 = toDiscreteWeights(10.0, new Double[]{0.005, 0.95, 0.045});
    printArray(weights1);
    
    Integer[] weights2 = toDiscreteWeights(10.0, new Double[]{0.8, 0.15, 0.05});
    printArray(weights2);
    
    Integer[] labels1 = toDiscreteDist(weights2);
    //Arrays.stream(labels1).forEach(System.out::println);
    System.out.println(labels1.length);
    
    Stream<Integer> stream8 = uniformSampler(weights2);
    //stream8.limit(10).forEach(p -> System.out.println(p));
    System.out.println( stream8.limit(200).filter(e -> e == 3).count() );

    double mean_1 = 1000;
    double sd_1   = 50;
    double mean_2 = 1200;
    double sd_2   = 60;
    Stream<Double> biModal1 = biModalDist(r, 10, new Double[]{0.5, 0.5}, mean_1, sd_1, mean_2, sd_2);

    // http://knowm.org/open-source/xchart/xchart-example-code/
    // https://github.com/timmolter/xchart
    // http://trac.erichseifert.de/vectorgraphics2d/
    // https://github.com/eseifert/vectorgraphics2d
    // Create Chart
    CategoryChart chart = new CategoryChartBuilder().width(800).height(600).title("Score Histogram").xAxisTitle("Mean").yAxisTitle("Count").build();
    // Customize Chart
    chart.getStyler().setLegendPosition(LegendPosition.InsideNW);
    chart.getStyler().setAvailableSpaceFill(.96);
    chart.getStyler().setOverlapped(true);
    // Data
    List<Double> data1 = biModal1.limit(10000).collect(Collectors.toList());
    
    // Add data graph to chart
    //Histogram histogram1 = new Histogram(data1, 20, -20, 20);
    Histogram histogram1 = new Histogram(data1, 20);
    chart.addSeries("histogram 1", histogram1.getxAxisData(), histogram1.getyAxisData());
    
    new SwingWrapper<CategoryChart>(chart).displayChart();
    try {
      VectorGraphicsEncoder.saveVectorGraphic(chart, "./output/bimiodal_1", VectorGraphicsFormat.SVG);
    } catch (IOException e){
      e.printStackTrace();
    }
    
    // https://docs.oracle.com/javase/tutorial/java/data/numberformat.html
  }
  
}

