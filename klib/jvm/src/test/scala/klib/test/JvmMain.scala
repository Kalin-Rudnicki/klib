package klib.test

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import javax.swing.*
import zio.*

import klib.utils.*
import klib.utils.commandLine.parse.*

object JvmMain extends ExecutableApp {

  object CopilotTest {

    import java.time.*
    import java.util.UUID

    import klib.utils.commandLine.parse.*

    object Database {

      trait HasId {
        val id: UUID
      }

      // =====| Types |=====

      final case class Musician(
          id: UUID,
          firstName: String,
          lastName: String,
          birthDate: LocalDate,
          deathDate: Option[LocalDate],
      ) extends HasId
      object Musician {

        def byId(id: UUID): KTask[Musician] =
          Tables.musicians.get(id) match {
            case Some(m) => ZIO.succeed(m)
            case None    => ZIO.failNEL(KError.Unexpected(s"Musician with id $id not found"))
          }

      }

      final case class Band(
          id: UUID,
          name: String,
          creationDate: LocalDate,
      ) extends HasId
      object Band {

        def byId(id: UUID): KTask[Band] =
          Tables.bands.get(id) match {
            case Some(b) => ZIO.succeed(b)
            case None    => ZIO.failNEL(KError.Unexpected(s"Band with id $id not found"))
          }

      }

      final case class MusicianInBand(
          id: UUID,
          musicianId: UUID,
          bandId: UUID,
      ) extends HasId
      object MusicianInBand {

        def byId(id: UUID): KTask[MusicianInBand] =
          Tables.musicianInBands.get(id) match {
            case Some(mib) => ZIO.succeed(mib)
            case None      => ZIO.failNEL(KError.Unexpected(s"MusicianInBand with id $id not found"))
          }

        def byBandId(bandId: UUID): KTask[List[MusicianInBand]] =
          ZIO.succeed {
            Tables.musicianInBands.valuesIterator
              .filter(_.bandId == bandId)
              .toList
          }

      }

      final case class Album(
          id: UUID,
          name: String,
          releaseYear: Int,
          bandId: UUID,
      ) extends HasId
      object Album {

        def byId(id: UUID): KTask[Album] =
          Tables.albums.get(id) match {
            case Some(a) => ZIO.succeed(a)
            case None    => ZIO.failNEL(KError.Unexpected(s"Album with id $id not found"))
          }

        def byBandId(bandId: UUID): KTask[List[Album]] =
          ZIO.succeed {
            Tables.albums.valuesIterator
              .filter(a => a.bandId == bandId)
              .toList
          }

      }

      enum SongBelongsTo {
        case Album(albumId: UUID)
        case Band(bandId: UUID)
      }

      final case class Song(
          id: UUID,
          name: String,
          songBelongsTo: SongBelongsTo,
      ) extends HasId
      object Song {

        def byId(id: UUID): KTask[Song] =
          Tables.songs.get(id) match {
            case Some(s) => ZIO.succeed(s)
            case None    => ZIO.failNEL(KError.Unexpected(s"Song with id $id not found"))
          }

        def byAlbumId(albumId: UUID): KTask[List[Song]] =
          ZIO.succeed {
            Tables.songs.valuesIterator
              .filter(s => s.songBelongsTo == SongBelongsTo.Album(albumId))
              .toList
          }

        def byBandId(bandId: UUID): KTask[List[Song]] =
          ZIO.succeed {
            Tables.songs.valuesIterator
              .filter(s => s.songBelongsTo == SongBelongsTo.Band(bandId))
              .toList
          }

      }

    }

    object Tables {

      private object Ids {

        object Musicians {
          val billyJoelId = UUID.randomUUID
          val bonJoviId = UUID.randomUUID
          val michaelJacksonId = UUID.randomUUID
          val richardSamboraId = UUID.randomUUID
          val kalinRudnickiId = UUID.randomUUID
        }

        object Bands {
          val billyJoelId = UUID.randomUUID
          val bonJoviId = UUID.randomUUID
          val michaelJacksonId = UUID.randomUUID
        }

        object MusicianInBands {
          val billyJoelId = UUID.randomUUID
          val bonJoviId = UUID.randomUUID
          val michaelJacksonId = UUID.randomUUID
          val richardSamboraId = UUID.randomUUID

          val kalinRudnickiInBillyJoelId = UUID.randomUUID
          val kalinRudnickiInBonJoviId = UUID.randomUUID
        }

        object Albums {
          val billyJoelId1 = UUID.randomUUID
          val billyJoelId2 = UUID.randomUUID
          val bonJoviId1 = UUID.randomUUID
          val bonJoviId2 = UUID.randomUUID
          val michaelJacksonId1 = UUID.randomUUID
          val michaelJacksonId2 = UUID.randomUUID
        }

      }

      private def makeTable[T <: Database.HasId](rows: T*): Map[UUID, T] =
        rows.map(r => r.id -> r).toMap

      val musicians: Map[UUID, Database.Musician] =
        makeTable(
          Database.Musician(
            Ids.Musicians.billyJoelId,
            "Billy",
            "Joel",
            LocalDate.of(1949, 5, 9),
            None,
          ),
          Database.Musician(
            Ids.Musicians.bonJoviId,
            "Bon",
            "Jovi",
            LocalDate.of(1962, 3, 2),
            None,
          ),
          Database.Musician(
            Ids.Musicians.michaelJacksonId,
            "Michael",
            "Jackson",
            LocalDate.of(1958, 8, 29),
            Some(LocalDate.of(2009, 6, 25)),
          ),
          Database.Musician(
            Ids.Musicians.richardSamboraId,
            "Richard",
            "Sambora",
            LocalDate.of(1959, 7, 11),
            None,
          ),
          Database.Musician(
            Ids.Musicians.kalinRudnickiId,
            "Kalin",
            "Rudnicki",
            LocalDate.of(1998, 7, 5),
            None,
          ),
        )

      val bands: Map[UUID, Database.Band] =
        makeTable(
          Database.Band(
            Ids.Bands.billyJoelId,
            "Billy Joel",
            LocalDate.of(1956, 1, 1),
          ),
          Database.Band(
            Ids.Bands.bonJoviId,
            "Bon Jovi",
            LocalDate.of(1962, 3, 2),
          ),
          Database.Band(
            Ids.Bands.michaelJacksonId,
            "Michael Jackson",
            LocalDate.of(1958, 8, 29),
          ),
        )

      val musicianInBands: Map[UUID, Database.MusicianInBand] =
        makeTable(
          Database.MusicianInBand(
            Ids.MusicianInBands.billyJoelId,
            Ids.Musicians.billyJoelId,
            Ids.Bands.billyJoelId,
          ),
          Database.MusicianInBand(
            Ids.MusicianInBands.bonJoviId,
            Ids.Musicians.bonJoviId,
            Ids.Bands.bonJoviId,
          ),
          Database.MusicianInBand(
            Ids.MusicianInBands.michaelJacksonId,
            Ids.Musicians.michaelJacksonId,
            Ids.Bands.michaelJacksonId,
          ),
          Database.MusicianInBand(
            Ids.MusicianInBands.richardSamboraId,
            Ids.Musicians.richardSamboraId,
            Ids.Bands.bonJoviId,
          ),
          Database.MusicianInBand(
            Ids.MusicianInBands.kalinRudnickiInBillyJoelId,
            Ids.Musicians.kalinRudnickiId,
            Ids.Bands.billyJoelId,
          ),
          Database.MusicianInBand(
            Ids.MusicianInBands.kalinRudnickiInBonJoviId,
            Ids.Musicians.kalinRudnickiId,
            Ids.Bands.bonJoviId,
          ),
        )

      val albums: Map[UUID, Database.Album] =
        makeTable(
          Database.Album(
            Ids.Albums.billyJoelId1,
            "Billy Joel Album 1",
            1900,
            Ids.Bands.billyJoelId,
          ),
          Database.Album(
            Ids.Albums.billyJoelId2,
            "Billy Joel Album 2",
            1900,
            Ids.Bands.billyJoelId,
          ),
          Database.Album(
            Ids.Albums.bonJoviId1,
            "Bon Jovi Album 1",
            1900,
            Ids.Bands.bonJoviId,
          ),
          Database.Album(
            Ids.Albums.bonJoviId2,
            "Bon Jovi Album 2",
            1900,
            Ids.Bands.bonJoviId,
          ),
          Database.Album(
            Ids.Albums.michaelJacksonId1,
            "Michael Jackson Album 1",
            1900,
            Ids.Bands.michaelJacksonId,
          ),
          Database.Album(
            Ids.Albums.michaelJacksonId2,
            "Michael Jackson Album 2",
            1900,
            Ids.Bands.michaelJacksonId,
          ),
        )

      val songs: Map[UUID, Database.Song] =
        makeTable(
          Database.Song(
            UUID.randomUUID,
            "Billy Joel Song 1 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.billyJoelId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Billy Joel Song 1 - 2",
            Database.SongBelongsTo.Album(Ids.Albums.billyJoelId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Billy Joel Song 2 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.billyJoelId2),
          ),
          Database.Song(
            UUID.randomUUID,
            "Billy Joel Song _ - 1",
            Database.SongBelongsTo.Band(Ids.Bands.billyJoelId),
          ),
          Database.Song(
            UUID.randomUUID,
            "Bon Jovi Song 1 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.bonJoviId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Bon Jovi Song 1 - 2",
            Database.SongBelongsTo.Album(Ids.Albums.bonJoviId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Bon Jovi Song 2 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.bonJoviId2),
          ),
          Database.Song(
            UUID.randomUUID,
            "Bon Jovi Song _ - 1",
            Database.SongBelongsTo.Band(Ids.Bands.bonJoviId),
          ),
          Database.Song(
            UUID.randomUUID,
            "Michael Jackson Song 1 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.michaelJacksonId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Michael Jackson Song 1 - 2",
            Database.SongBelongsTo.Album(Ids.Albums.michaelJacksonId1),
          ),
          Database.Song(
            UUID.randomUUID,
            "Michael Jackson Song 2 - 1",
            Database.SongBelongsTo.Album(Ids.Albums.michaelJacksonId2),
          ),
          Database.Song(
            UUID.randomUUID,
            "Michael Jackson Song _ - 1",
            Database.SongBelongsTo.Band(Ids.Bands.michaelJacksonId),
          ),
        )

    }

    object Code {

      final case class Musician(
          firstName: String,
          lastName: String,
          birthDate: LocalDate,
          deathDate: Option[LocalDate],
      )
      object Musician {

        def fromDatabase(m: Database.Musician): Musician =
          Musician(
            m.firstName,
            m.lastName,
            m.birthDate,
            m.deathDate,
          )

      }

      final case class Band(
          name: String,
          creationDate: LocalDate,
          musicians: List[Musician],
          albums: List[Album],
          singles: List[Song],
      )
      object Band {

        def fromDatabase(b: Database.Band): KTask[Band] =
          for {
            dbMusiciansInBand <- Database.MusicianInBand.byBandId(b.id)
            dbMusicians <- ZIO.foreach(dbMusiciansInBand)(mib => Database.Musician.byId(mib.musicianId))
            dbAlbums <- Database.Album.byBandId(b.id)
            dbSingles <- Database.Song.byBandId(b.id)

            muscians = dbMusicians.map(Musician.fromDatabase)
            albums <- ZIO.foreach(dbAlbums)(Album.fromDatabase)
            singles = dbSingles.map(Song.fromDatabase)
          } yield Band(
            b.name,
            b.creationDate,
            muscians,
            albums,
            singles,
          )

      }

      final case class Album(
          name: String,
          releaseYear: Int,
          songs: List[Song],
      )
      object Album {

        def fromDatabase(a: Database.Album): KTask[Album] =
          for {
            songs <- Database.Song.byAlbumId(a.id).map(_.map(Song.fromDatabase))
          } yield Album(
            a.name,
            a.releaseYear,
            songs,
          )

      }

      final case class Song(
          name: String,
      )
      object Song {

        def fromDatabase(s: Database.Song): Song =
          Song(
            s.name,
          )

      }

    }

  }

  override val executable: Executable =
    Executable
      .fromParser(Parser.unit.disallowExtras)
      .withLayer(_ => ZLayer.succeed(()))
      .withExecute { _ =>
        for {
          bandIds <- ZIO.kAttempt("...")(CopilotTest.Tables.bands.keySet.toList)
          dbBands <- ZIO.foreach(bandIds)(id => CopilotTest.Database.Band.byId(id))
          bands <- ZIO.foreach(dbBands)(CopilotTest.Code.Band.fromDatabase)
        } yield ()
      }

}
