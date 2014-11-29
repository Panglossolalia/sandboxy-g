package music

import javax.sound.sampled.AudioSystem
import java.io.File
import javax.sound.sampled.SourceDataLine
import javax.sound.sampled.DataLine
import javax.sound.sampled.AudioFormat
import math._
// Starting place/inspiration: http://vigtig.it/blog/blog/2011/04/12/programming-music/#sthash.XUKwXLWw.dpuf
object Music {
  val SAMPLE_RATE = 96000f
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
    println(af)
    sdl.open(af)

    sdl.start()
    var fr = FrereJacques()
    val modSaw = ModInst(Oscillator(saw),Oscillator(sine),64)
    fr.play(OverlayInst(modSaw,Vector(1,2)))
    fr.play(OverlayInst(Oscillator(square),Vector(1,2,4,8,16)))
    fr.play(Oscillator(saw))
    fr.play(Oscillator(square))
    fr.play(Oscillator(sine))
    sdl.drain()
    sdl.stop()
    sdl.close()

  }
}

trait Phrase {
  def play(inst: Instrument)
}

trait Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double): Double
}

case class Oscillator(of: Function1[Double, Double]) extends Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double): Double = {
    val angle = elem.toDouble / (sampleRate / hertz) * 2.0 * Pi
    of(angle)
  }
}

case class OverlayInst(inner: Instrument, downs: IndexedSeq[Int]) extends Instrument {

  def apply(elem: Int, hertz: Double, sampleRate: Double) = {
    var out = 0.0
    for (d <- downs) {
      out = out + inner(elem, hertz / d, sampleRate)
    }
    out/downs.size
  }
}

case class ModInst(main: Instrument, mod: Instrument, modHz: Double) extends Instrument {
  def apply(elem: Int, hertz: Double, sampleRate: Double) = {
    main.apply(elem,hertz,sampleRate)*mod(elem,modHz,sampleRate)
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