package music

import plotting.PlotIt

import javax.sound.sampled.AudioSystem
import java.io.File
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.DataLine
import javax.sound.sampled.{ AudioFormat, AudioInputStream }
import java.io.ByteArrayOutputStream
import math._
// Starting place/inspiration: http://vigtig.it/blog/blog/2011/04/12/programming-music/#sthash.XUKwXLWw.dpuf
object Music {
  val SAMPLE_RATE = 64000f
  //96khz for audiophiles 
  val BYTE_BUFFER_SIZE = 1000
  val buf: Array[Byte] = Array.ofDim(BYTE_BUFFER_SIZE)
  val af = new AudioFormat(SAMPLE_RATE, 8, 1, true, false)
  val sdl = AudioSystem.getSourceDataLine(af)

  //Oscillators 
  def square(d: Double): Double = if (sin(d) < 0) -1 else 1
  def sine(d: Double): Double = sin(d)
  def saw(d: Double): Double = (Pi - (d % (Pi * 2))) / Pi

  //Returns hertz for nth key 
  def key(n: Int): Int = (440f * pow((pow(2, 1 / 12f)), n - 49 - 12)).toInt

  val (c, d, e, f, g, a, gdeep) = (key(52), key(54), key(56), key(57), key(59), key(61), key(47))

  def main(args: Array[String]) {
    val samp = loadSample("data/Warship alarm.wav")
    val samp2 = loadSample("data/Death 1.wav")
    println(af)
    sdl.open(af)
    val organ = OverlayInst(Oscillator(sin), Vector(1, 2, 4, 8, 16))
    val organSamp = OverlayInst(samp, Vector(1, 2, 4, 8, 16))
    
    val distBound = .3
    def distFilt(d: Double) = { 2.0 * (if (d > distBound) distBound else if (d < -distBound) -distBound else d) }

    val distOrgan = FilterInst(organ, distFilt)
    val distOrganSamp = FilterInst(organSamp, distFilt)
    val distSamp = FilterInst(organ, distFilt)
    val modDistOrgan = ModInst(distOrgan, Oscillator(sine), 16)
    val modSaw = ModInst(Oscillator(sine), Oscillator(sine), 64)

    sdl.start()
    var mfr = MiniFr()
    var fr = FrereJacques()
    
    fr.play(distOrganSamp)
    mfr.play(distOrgan)
    mfr.play(modDistOrgan)
    /*
    fr.play(Oscillator(saw))
    fr.play(Oscillator(square))
    fr.play(Oscillator(sine))
    * 
    */
    sdl.drain()
    sdl.stop()
    sdl.close()

  }

  def loadSample(path: String) = {
    import javax.sound.sampled._

    val stream = AudioSystem.getAudioInputStream(new File(path));
    val fmt = stream.getFormat
    val sampleRate = fmt.getSampleRate
    println(s"Loading ${path}, format=${fmt} frameLength=${stream.getFrameLength}")

    val bos = new ByteArrayOutputStream();
    val byteArray = readAudioFileData(stream)
    println(s"Got array of ${byteArray.size} bytes")

    val wrapped = java.nio.ByteBuffer.wrap(byteArray); // big-endian by default
    //val num = wrapped.getShort(); // 1
    val doubles = audioToDoubles(byteArray, fmt)

    //println(s"Got doubles, length ${doubles.size}, max ${doubles.max}, min ${doubles.min}")
    //plotting.PlotIt.plot(audioToDoubles(byteArray,fmt))
    SampleInst(doubles, 300, fmt.getSampleRate)
  }

  def audioToDoubles(buffer: Array[Byte], format: AudioFormat) = {

    val ints = format match {
      case f if (f.getFrameSize == 2 && f.isBigEndian() == false) => {
        assert(f.getFrameSize % 2 == 0)
        for (i <- 0 until buffer.size / 2) yield {
          val offset = 2 * i
          (buffer(offset) & 0xFF) | (buffer(offset + 1) << 8);
        }
      }
    }
    val max = ints.max.toDouble
    ints.map(_.toDouble / max)

  }

  def readAudioFileData(ais: AudioInputStream) = {
    var data = Array.empty[Byte]
    val baout = new ByteArrayOutputStream()

    val buffer = new Array[Byte](4096);
    var c = 0
    while ({ c = ais.read(buffer, 0, buffer.length); c != -1 }) {
      baout.write(buffer, 0, c);
    }
    ais.close();
    baout.close();
    baout.toByteArray();
  }
}

trait Phrase {
  def play(inst: Instrument)
}

trait Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double): Double
}

case class SampleInst(waveForm: IndexedSeq[Double], encodeHz: Double, encodeSampleRate: Double) extends Instrument {
  // If the hz and samplerate are matches, then just march through
  def apply(elem: Int, hertz: Double, sampleRate: Double): Double = {
    val idx = elem * (hertz / encodeHz) * (encodeSampleRate / sampleRate)
    waveForm((idx % waveForm.size).toInt)
  }
}

case class Oscillator(of: Function1[Double, Double]) extends Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double): Double = {
    val angle = elem.toDouble / (sampleRate / hertz) * 2.0 * Pi
    of(angle)
  }
}

case class FilterInst(inner: Instrument, filt: Function1[Double, Double]) extends Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double) = {
    filt(inner(elem, hertz, sampleRate))
  }
}

case class OverlayInst(inner: Instrument, downs: IndexedSeq[Int]) extends Instrument {

  def apply(elem: Int, hertz: Double, sampleRate: Double) = {
    var out = 0.0
    for (d <- downs) {
      out = out + inner(elem, hertz / d, sampleRate)
    }
    out / downs.size
  }
}

case class ModInst(main: Instrument, mod: Instrument, modHz: Double) extends Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double) = {
    main.apply(elem, hertz, sampleRate) * mod(elem, modHz, sampleRate)
  }
}

case class MiniFr() extends Phrase {
  import Music._

  val tones = Vector(
    Tone(gdeep),
    Tone(c),
    Tone(d),
    Tone(e),
    Tone(f),
    Tone(g),
    Tone(a))

  def play(osc: Instrument) = {
    tones.foreach(_.play(osc))
  }
}

case class FrereJacques() extends Phrase {
  import Music._
  val tones = Vector(Tone(c),
    Tone(d),
    Tone(e),
    Tone(c),
    Tone(c),
    Tone(d),
    Tone(e),
    Tone(c),
    Tone(e),
    Tone(f),
    Tone(g, 1),
    Tone(e),
    Tone(f),
    Tone(g, 1),
    Tone(g, 4),
    Tone(a, 4),
    Tone(g, 4),
    Tone(f, 4),
    Tone(e),
    Tone(c),
    Tone(g, 4),
    Tone(a, 4),
    Tone(g, 4),
    Tone(f, 4),
    Tone(e),
    Tone(c),
    Tone(c),
    Tone(gdeep),
    Tone(c, 1),
    Tone(c),
    Tone(gdeep),
    Tone(c, 1))

  def play(osc: Instrument) = {
    tones.foreach(_.play(osc))
  }
}

case class Tone(hertz: Int, speed: Double = 2) {
  import Music._
  def play(osc: Instrument) = {
    for (i <- 0 until (SAMPLE_RATE / (speed * 2)).toInt by BYTE_BUFFER_SIZE) {
      for (j <- 0 until BYTE_BUFFER_SIZE) {
        val angle = (i + j) / (SAMPLE_RATE / hertz) * 2.0 * Pi
        buf(j) = (osc(i + j, hertz, SAMPLE_RATE) * 100).toByte
        val left = (sdl.available - sdl.getBufferSize)
      }
      sdl.write(buf, 0, BYTE_BUFFER_SIZE)
    }
  }
}