/*
 * Copyright (c) 2019 Mathias Doenitz
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package io.bullet.borer.internal

import java.nio.charset.StandardCharsets.UTF_8

import utest._

object ReceptacleSpec extends TestSuite {

  case class Str(string: String, bytes: Array[Byte])

  val fragments = Seq("", "foo", "123456", "árvíztűrő ütvefúrógép!", "เอส เอฟ", "飞机因此受到")

  val strings = for {
    a <- fragments
    b <- fragments
    c <- fragments
  } yield {
    val s = s"$a$b$c"
    Str(s, s.getBytes(UTF_8))
  }
  val maxLen    = strings.maxBy(_.bytes.length).bytes.length
  val byteArray = new Array[Byte](maxLen + 16)

  val tests = Tests {

    "stringCompare(String) on String" - testCompare { (r, a, b) =>
      r.onString(a.string)
      r.stringCompare(b.string)
    }

    "stringCompare(Array[Byte]) on String" - testCompare { (r, a, b) =>
      r.onString(a.string)
      r.stringCompare(b.bytes)
    }

    "stringCompare(String) on byte array" - testCompare { (r, a, b) =>
      val ix = System.nanoTime().toInt & 0xF
      System.arraycopy(a.bytes, 0, byteArray, ix, a.bytes.length)
      r.onString(byteArray, ix, a.bytes.length, utf8 = true)
      r.stringCompare(b.string)
    }

    "stringCompare(Array[Byte]) on byte array" - testCompare { (r, a, b) =>
      val ix = System.nanoTime().toInt & 0xF
      System.arraycopy(a.bytes, 0, byteArray, ix, a.bytes.length)
      r.onString(byteArray, ix, a.bytes.length, utf8 = true)
      r.stringCompare(b.bytes)
    }

    "textCompare(String)" - testCompare { (r, a, b) =>
      r.onText(a.bytes)
      r.textCompare(b.string)
    }

    "textCompare(Array[Byte])" - testCompare { (r, a, b) =>
      r.onText(a.bytes)
      r.textCompare(b.bytes)
    }
  }

  def testCompare(f: (Receptacle, Str, Str) => Int): Unit =
    for {
      a <- strings
      b <- strings
    } test(a, b)(f)

  def test(a: String, b: String)(f: (Receptacle, Str, Str) => Int): Unit =
    test(Str(a, a getBytes UTF_8), Str(b, b getBytes UTF_8))(f)

  def test(a: Str, b: Str)(f: (Receptacle, Str, Str) => Int): Unit = {
    val receptacle        = new Receptacle
    val receptacleCompare = math.signum(f(receptacle, a, b))
    val stringCompare     = math.signum(a.string compareTo b.string)
    if (receptacleCompare != stringCompare) {
      throw new java.lang.AssertionError(
        s"""receptacleCompare: $receptacleCompare for "${a.string}" <?> "${b.string}"""")
    }
  }
}
