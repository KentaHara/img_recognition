import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._

object FileTools extends Init{
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
  
  def writeCsv(data: Mat, filename: String): Unit = {
    import java.io.{FileOutputStream=>FileStream, OutputStreamWriter=>StreamWriter}
    val writer = new StreamWriter( new FileStream(filename, false), "UTF-8")
    for(y <- 0 until data.height.toInt) {
      for(x <- 0 until data.width.toInt) {
        data.get(y, x).foreach(s => writer.write(s.toString+","))
      }
      writer.write("\n")
    }
    writer.close
  }

  def readCsv(filename: String): Mat = {
    import scala.io.Source
    val source = Source.fromFile(filename, "UTF-8")
    var list = List(Array.empty[String])
    source.getLines.foreach( s => {
      list = list:::List(s split ',')
    })
    var result = new Mat(list.size-1, list.apply(1).length, CvType.CV_32FC1) 
    for(y <- 1 until list.size) {
      for(x <- 0 until list.apply(y).length) {
        result.put(y-1, x, list.apply(y)(x).toDouble)
      }
    }
    return result
  }

  def writeOptionCsvImg(data: OptionImgList, filename: String): Unit = {
    import java.io.{FileOutputStream=>FileStream, OutputStreamWriter=>StreamWriter}
    val writer = new StreamWriter( new FileStream(filename, false), "UTF-8")
    data.keys.foreach( key => {
      val m: Mat = data.get(key).get.get
      writer.write(key + ",")
      for(y <- 0 until m.height.toInt) {
        for(x <- 0 until m.width.toInt) {
          m.get(y, x).foreach(s => writer.write(s.toString+","))
        }
        writer.write("\n")
      }
    })
    writer.close
  }

  def writeOptionCsvScore(data: OptionScoreList, filename: String): Unit = {
    import java.io.{FileOutputStream=>FileStream, OutputStreamWriter=>StreamWriter}
    val writer = new StreamWriter( new FileStream(filename, false), "UTF-8")
    data.keys.foreach( key => {
      writer.write(key + "," + data.get(key).get.get.toString + ",\n")
    })
    writer.close
  }

  def readOptionCsvImg(filename: String): OptionImgList = {
    import scala.io.Source
    val source = Source.fromFile(filename, "UTF-8")
    var list = List(Array.empty[String])
    source.getLines.foreach( s => {
      list = list:::List(s split ',')
    })

    var result:OptionImgList = InitOptionImgList
    for(y <- 1 until list.size) {
      val m = new Mat(1, list.apply(1).length, CvType.CV_32FC1) 
      for(x <- 1 until list.apply(y).length) {
        m.put(y-1, x, list.apply(y)(x).toDouble)
      }
      val key = list.apply(y)(0).toString
      result += key -> Some(m)
    }
    return result 
  }
  def readOptionCsvScore(filename: String): OptionScoreList = {
    import scala.io.Source
    val source = Source.fromFile(filename, "UTF-8")
    var list = List(Array.empty[String])
    source.getLines.foreach( s => {
      list = list:::List(s split ',')
    })

    var result:OptionScoreList = InitOptionScoreList
    for(y <- 1 until list.size) {
      val key   = list.apply(y)(0).toString
      val value = list.apply(y)(1).toString
      result += key -> Some(value.toDouble)
    }
    return result 
  }
}
