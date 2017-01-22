import org.opencv.core.{Core, Mat, CvType, Scalar, Size, Rect}
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import java.io._

class Init {

  type OptionImgList      = Map[String, Option[Mat]]
  val InitOptionImgList   = Map[String, Option[Mat]]()
  type OptionScoreList    = Map[String, Option[Double]]
  val InitOptionScoreList = Map[String, Option[Double]]()

  val size         = new Size(320, 50)
  val bunner_size  = new Size(320, 50)
  val test_img_dir = "img"
  val mean_file_name = "mean.csv"
  val eigenvector_file_name = "eigenvector.csv"
  val model_uri = getClass.getResource("/model/").getPath
  def getResourceFileName(dir: String): List[String] = {
    val uri = getClass.getResource("/"+dir+"/").getPath
    val filelist = new File(uri).list
    var list = List[String]()
    filelist.foreach(s=>{list = (uri+s)::list})
    return list
  }
  def getResourceFileNameMap(dir: String): Map[String, String] = {
    val uri = getClass.getResource("/"+dir+"/").getPath
    val filelist = new File(uri).list
    var map = Map[String, String]()
    filelist.foreach(s=>{map += s->(uri+s)})
    return map
  }
}

object TestImages extends Init {

  def getImages(): List[Mat] = {
    var matList = List[Mat]()
    getResourceFileName(test_img_dir).foreach(filePath => {
      val src = Highgui.imread(filePath, 0)
      if(src.cols > 0) {
        Imgproc.resize(src, src, size)
        matList = matList :+ MatTool.convertOneDimensionMatFromMat(src)
      }
    })
    return matList
  }
  
  def getOptionImages(): OptionImgList = {
    var mat_list = InitOptionImgList
    val filename_list = getResourceFileNameMap(test_img_dir)
    filename_list.keys.foreach(key => {
      val filePath = filename_list.get(key).get
      println(filePath)
      val src = Highgui.imread(filePath, 0)
      if(src.cols > 0) {
        Imgproc.resize(src, src, size)
        mat_list += key -> Option(MatTool.convertOneDimensionMatFromMat(src))
      }
    })
    return mat_list
  }
}

object TrainingModel extends Init {
  def main(args:Array[String]) {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    
    val matList = TestImages.getOptionImages()

    val imgMat = MatTool.convertMapFromOneDimensionMats(matList)
    var mean = new Mat()
    var eigenvector = new Mat()
    val result = Core.PCACompute(imgMat, mean, eigenvector, imgMat.height )

    // output mean image
    val model_uri = getClass.getResource("/model/").getPath
    val mean_img = MatTool.convertMatFromOneDimensionMat(mean, size)
    Imgproc.resize(mean_img, mean_img, bunner_size)
    Highgui.imwrite(model_uri + "mean_img.bmp", mean_img)
    
    // output eigen image
    for(y <- 0 until eigenvector.height) {
      var eigen = new Mat
      Core.normalize(eigenvector.row(y), eigen, 0, 255, Core.NORM_MINMAX)
      val eigen_img = MatTool.convertMatFromOneDimensionMat(eigen, size)
      Imgproc.resize(eigen_img, eigen_img, bunner_size)
      Highgui.imwrite(model_uri+"eigen_"+y.toString+".bmp", eigen_img)
    }

    FileTools.writeCsv(mean, model_uri + "mean.csv")
    FileTools.writeCsv(eigenvector, model_uri + "eigenvector.csv")
 }
}


