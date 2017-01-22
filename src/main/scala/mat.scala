import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._


object MatTool extends Init{
  def convertMapFromOneDimensionMats(matList: List[Mat]): Mat = {
    var result = new Mat()
    matList.foreach( s=> {
      result = combineMat(result, s, "CV_Y")
    })
    return result
  }

  def convertMapFromOneDimensionMats(matList: OptionImgList): Mat = {
    var result = new Mat()
    matList.keys.foreach( key => {
      matList.get(key).get match {
        case Some(m) => result = combineMat(result, m, "CV_Y")
        case None    => None
      }
    })
    return result
  }

  def convertOneDimensionMatFromMat(data: Mat): Mat = {
    var result = Mat.zeros(0, 0, CvType.CV_8UC1)
    for(y <- 0 until data.rows) {
      result = combineMat(result, data.row(y), "CV_X")
    }
    return result
  }

  def convertMatFromOneDimensionMat(data: Mat, size: Size): Mat = {
    var result = new Mat()
    var convert_data = new Mat()
    data.convertTo(convert_data, CvType.CV_8UC1)
    for(y <- 0 until size.height.toInt) {
      val roi_size = new Rect(y*size.width.toInt, 0, size.width.toInt, 1)
      val roi = convert_data.submat(roi_size)
      result = combineMat(result, roi, "CV_Y") 
    }
    return result
  }
  
  def combineMat(base: Mat, target: Mat, combine_flg: String): Mat = {
    combine_flg match {
      case "CV_X" => return combineMatx(base, target)
      case "CV_Y" => return combineMaty(base, target)
    }
    return new Mat
  }

  def combineMatx(base: Mat, target:Mat): Mat = {
    var result = Mat.zeros(target.height, base.width+target.width, CvType.CV_8UC1)
    for(x <- 0 until base.width)   base.col(x).copyTo(result.col(x))
    for(x <- 0 until target.width) target.col(x).copyTo(result.col(x+base.width))
    return result
  }

  def combineMaty(base: Mat, target:Mat): Mat = {
    var result = Mat.zeros(base.height+target.height, target.width, CvType.CV_8UC1)
    for(y <- 0 until base.height)   base.row(y).copyTo(result.row(y))
    for(y <- 0 until target.height) target.row(y).copyTo(result.row(y+base.height))
    return result
  }

}
