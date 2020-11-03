package edu.colorado.csci3155.project2
import scala.math.{cos, pow, sin, sqrt}

/* A class to maintain a canvas. */
import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def rotate(angle: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {

    override def rotate(angle: Double): Polygon = {
        val p = cList.map(c => ((c._1 * cos(angle)) - (c._2 * sin(angle)), (c._1 * sin(angle)) + (c._2 * cos(angle))))
        Polygon(p)
    }
    //TODO: Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xMin = cList.minBy(_._1)._1
        val xMax = cList.maxBy(_._1)._1
        val yMin = cList.minBy(_._2)._2
        val yMax = cList.maxBy(_._2)._2
        (xMin, xMax, yMin, yMax)
    }

    //TODO: Create a new polygon by shifting each vertex in cList by (x,y)
    //    Do not change the order in which the vertices appear
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        val p = cList.map(c => (c._1 + shiftX, c._2 + shiftY))
        Polygon(p)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {

     override def rotate(angle: Double): MyCircle ={
         val X = (c._1 * cos(angle)) - (c._2 * sin(angle))
         val Y = (c._1 * sin(angle)) + (c._2 * cos(angle))
         MyCircle((X,Y), r)
    }

    //TODO: Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = c._1 - r
        val xMax = c._1 + r
        val yMin = c._2 - r
        val yMax = c._2 + r
        (xMin, xMax, yMin, yMax)
    }


    //TODO: Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        val coordinates: (Double, Double) = (c._1 + shiftX, c._2 + shiftY)
        MyCircle(coordinates, r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // TODO: Write a function to get the boundingbox for the entire canvas.
    // Hint: use existing boundingbox functions defined in each figure.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = listOfObjects.map(x => x.getBoundingBox).minBy(_._1)._1
        val xMax = listOfObjects.map(x => x.getBoundingBox).maxBy(_._2)._2
        val yMin = listOfObjects.map(x => x.getBoundingBox).minBy(_._3)._3
        val yMax = listOfObjects.map(x => x.getBoundingBox).maxBy(_._4)._4
        (xMin, xMax, yMin, yMax)
    }

    //TODO: Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        val figs: List[Figure] = listOfObjects.map(x => x.translate(shiftX, shiftY))
        new MyCanvas(figs)
    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val c1 = this.getBoundingBox
        val c2 = myc2.getBoundingBox
        val newX = c1._2 - c1._1
        val newY = ((c1._4 - c1._3)/2) - ((c2._4 - c2._3)/2)
        val c3 = myc2.translate(newX, newY)
        this.overlap(c3)

    }

    //TODO: Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    // refer to the notebook documentation on how to perform this.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val c1 = this.getBoundingBox
        val c2 = myc2.getBoundingBox
        val newX = ((c1._2 - c1._1)/2) - ((c2._2 - c2._1)/2)
        val newY = c1._4 - c1._3
        val c3 = myc2.translate(newX, newY)
        this.overlap(c3)
    }

    //TODO: Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    // Suggestion: first write rotation functions for polygon and circle.
    //             those functions have not been added in the classes but you can do so with the
    //             appropriate signature.
    // rotating a polygon is simply rotating each vertex.
    // rotating a circle is simply rotating the center with radius unchanged.


    def rotate(angRad: Double): MyCanvas = {
        val o: List[Figure] = listOfObjects.map(f => f.rotate(angRad))
        val c: MyCanvas = new MyCanvas(o)
        c
    }

    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
