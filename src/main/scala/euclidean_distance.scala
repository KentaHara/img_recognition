import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect, Range}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._
import Math.{sqrt, pow}
import scala.collection.immutable.Map

class EulideanDistance extends Init{
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME);

  def matDistance(src: Mat, cmp: Mat): Option[Long] = {
    if(src.cols != cmp.cols || src.rows != cmp.rows){
      return None
    }
    var result = Mat.zeros(src.rows, src.cols, CvType.CV_32FC1)

    for(x <- 0 until src.rows) {
      for(y <- 0 until src.cols) {
        val distance =  sqrt(pow(2*src.get(x, y)(0), 2) + pow(2*cmp.get(x, y)(0), 2))
        result.put(x, y, distance)
      }
    }
    return Some(result.total)
  }
  
  def makeDistance(src: OptionImgList, cmp: Mat): OptionScoreList = {
    var result: OptionScoreList = InitOptionScoreList
    src.keys.foreach(i => {
      src.get(i).get match {
        case Some(m) => result += i -> Some(Core.norm(m, cmp, Core.NORM_L2))
        case None    => result += i -> None
      }
    })
    return result
  }

  def subAverageImg(src: Mat, ave: Mat): Option[Mat] = {
    var result: Mat = new Mat(src.size, CvType.CV_32FC1)
    src.convertTo(src, CvType.CV_32FC1)
    ave.convertTo(ave, CvType.CV_32FC1)
    Core.subtract(src, ave, result)
    return Some(result)
  }

  def makeSubAverageOptionImgList(src: OptionImgList, ave: Mat): OptionImgList = {
    var result: OptionImgList = InitOptionImgList
    src.keys.foreach(i => {
      src.get(i).get match {
        case Some(m) => result += i -> subAverageImg(m, ave)
        case None    => result += i -> None
      }
    })
    return result
  }
  
  def makeScore(src: Mat, eigen: Mat, use: Int): Option[Mat] = {
    src.convertTo(src, CvType.CV_32FC1)
    eigen.convertTo(eigen, CvType.CV_32FC1)
    val range = new Range(0, use)
    val use_eigen = eigen.rowRange(range)
    val u = multiply(use_eigen, use_eigen.t, CvType.CV_32FC1)
    u match {
      case Some(m) => return multiply(m, src, CvType.CV_32FC1)
      case None    => return None
    }
  }

  def multiply(src: Mat, cmp: Mat, types: Int): Option[Mat] = {
    val result = Mat.zeros(cmp.rows, src.cols, types)
    for(i <- 0 until src.cols; j <- 0 until src.rows; k <- 0 until cmp.rows) {
        val value = src.get(j, i)(0) * cmp.get(k, j)(0) + result.get(k, i)(0)
        result.put(k, i, value)
    }
    return Some(result)
  }
  def makeScoreList(src: OptionImgList, eigen: Mat, use: Int): OptionImgList = {
    var result: OptionImgList = InitOptionImgList
    src.keys.foreach(i => {
      src.get(i).get match {
        case Some(m) => result += i -> makeScore(m, eigen, use)
        case None    => result += i -> None
      }
    })
    return result
  }

  def convertMapFromOptionMap(m: OptionScoreList): Map[String, Double] = {
    var result: Map[String, Double] = Map[String, Double]()
    m.keys.foreach(i => {
      m.get(i).get match {
        case Some(m) => result += i->m
        case None => None
      }
    })
    return result
  }

  def detection(m: OptionImgList, eigen: Mat, ave: Mat, target: OptionImgList): Unit = {
    var start = System.currentTimeMillis()
    
    val range: Int = 1

    val sub_average_img_csv = "sub_average_img.csv"
    val score_csv = "score.csv"

    val sub_average_img = makeSubAverageOptionImgList(m, ave)
    FileTools.writeOptionCsvImg(sub_average_img, model_uri + sub_average_img_csv)
    printExecutionTime(start, "makeSubAverageOptionImgList")

    val score = makeScoreList(sub_average_img, eigen, range)
    FileTools.writeOptionCsvImg(score, model_uri + score_csv)
    printExecutionTime(start, "makeScoreList")

    var result = Map[String, String]()
    target.keys.foreach( key => {
      val img = target.get(key).get.get
      val sub_average_img_target = makeSubAverageOptionImgList(Map(key -> Some(img)), ave)
      FileTools.writeOptionCsvImg(sub_average_img_target, model_uri + key + "." + sub_average_img_csv)
      //printExecutionTime(start, "makeSubAverageOptionImgList")
      val score_target = makeScoreList(sub_average_img_target, eigen, range)
      FileTools.writeOptionCsvImg(score_target, model_uri + key + "." + score_csv)
      //printExecutionTime(start, "makeScoreList")

      var distance: OptionScoreList = InitOptionScoreList
      score_target.keys.foreach( key => {
        score_target.get(key).get match {
          case Some(cmp) => distance = makeDistance(score, cmp)
          case None      => distance = Map( "None" -> None )
        }
      })
      FileTools.writeOptionCsvScore(distance, model_uri + key + ".distance.csv")
      //printExecutionTime(start, "makeDistance")
      val dis = convertMapFromOptionMap(distance)
      result += key -> dis.minBy(_._2).toString
      //println(key + "->" + dis.minBy(_._2))
    })
    printExecutionTime(start, "allTester")
    result.keys.foreach( key => println(key +" -> " + result(key)))
  }
  def printExecutionTime(start: Long, method: String): Unit = {
    println("[" + method + "] " + (System.currentTimeMillis - start) + "msec")
  }
}

object EulideanDistanceTester extends EulideanDistance{

  def main(args: Array[String]) {

    val mean        = FileTools.readCsv(model_uri + mean_file_name)
    val eigenvector = FileTools.readCsv(model_uri + eigenvector_file_name)
    val test_img   = TestImages.getOptionImages
    detection(test_img, eigenvector, mean, test_img)
  }
}
