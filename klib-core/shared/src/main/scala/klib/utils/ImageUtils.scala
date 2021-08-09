package klib.utils

import java.io.File
import javax.imageio._
import javax.imageio.stream._
import javax.imageio.metadata._
import org.w3c.dom._

import klib.Implicits._
import klib.fp.types._

object ImageUtils {

  sealed trait MetaDataNode {

    val name: String

    def toIdtString: IndentedString = {
      import IndentedString._

      this match {
        case MetaDataNode.Node(name, attributes, children) =>
          val childIdtStrs =
            children.map(_.toIdtString)

          inline(
            s"[Node] $name:",
            indented(
              attributes.toNel match {
                case Some(attributes) =>
                  inline(
                    "[Attributes]:",
                    indented(
                      attributes.toList.map {
                        case (name, value) =>
                          s"$name: $value"
                      },
                    ),
                    "[Children]:",
                    indented(childIdtStrs),
                  )
                case None =>
                  inline(childIdtStrs)
              },
            ),
          )
        case MetaDataNode.Leaf(name, attributes) =>
          inline(
            s"[Leaf] $name:",
            indented(
              attributes.map {
                case (name, value) =>
                  s"$name: $value"
              },
            ),
          )
        case MetaDataNode.ValueLeaf(name, value) =>
          s"[ValueLeaf] $name: $value"
      }
    }

  }
  object MetaDataNode {
    final case class Node(name: String, attributes: List[(String, String)], children: List[MetaDataNode])
        extends MetaDataNode
    final case class Leaf(name: String, attributes: List[(String, String)]) extends MetaDataNode
    final case class ValueLeaf(name: String, value: String) extends MetaDataNode
  }

  def getMetaData(file: File): IO[Unit] = {
    def convertNode(node: Node): MetaDataNode = {
      val name = node.getNodeName
      val attributes =
        Maybe(node.getAttributes)
          .map { attrMap =>
            0.until(attrMap.getLength).toList.map { i =>
              val node = attrMap.item(i)
              (node.getNodeName, node.getNodeValue)
            }
          }
          .flatMap(_.toNel)
      val children =
        Maybe(node.getChildNodes)
          .map { childNodes =>
            0.until(childNodes.getLength).toList.map { i =>
              convertNode(childNodes.item(i))
            }
          }
          .flatMap(_.toNel)

      children match {
        case Some(children) =>
          MetaDataNode.Node(name, attributes.cata(_.toList, Nil), children.toList)
        case None =>
          attributes match {
            case Some(NonEmptyList(("value", value), Nil)) =>
              MetaDataNode.ValueLeaf(name, value)
            case _ =>
              MetaDataNode.Leaf(name, attributes.cata(_.toList, Nil))
          }
      }
    }

    for {
      iis <- ImageIO.createImageInputStream(file).pure[IO]
      readers <- ImageIO.getImageReaders(iis).pure[IO]
      reader <- readers.hasNext ? readers.next.pure[IO] | IO.error(Message("No Readers"))
      _ = reader.setInput(iis, true)
      metadata = reader.getImageMetadata(0)
      names = metadata.getMetadataFormatNames
      nodes = names.map(metadata.getAsTree)
      rootNode = MetaDataNode.Node(
        name = "ROOT",
        attributes = Nil,
        children = nodes.map(convertNode).toList,
      )
      _ = println(rootNode.toIdtString.toString("|   "))
    } yield () // TODO (KR) :
  }

}
