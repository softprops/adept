package adept

import org.scalatest.FunSpec

class ManagerSpec extends FunSpec {
  val testRepo = "git://github.com/softprops/testmeta.git"
  describe("A Manager") {
    it ("should add repos") {
      assert(Manager.repos.add(testRepo) === Right("Cloned repo to %s/testmeta" format Config.reposDir))
    }

    it ("should not same add repo twice") {
      assert(Manager.repos.add(testRepo) === Left("repo %s already added" format testRepo))
    }

    it ("should list repos") {
      assert(Manager.repos.list === List("testmeta"))
    }

    it ("should be updatable") {
      assert(Manager.repos.update === Right("repos updated"))
    }

    it ("should list modules") {
      assert(Manager.modules.list === List(("semverfi",Seq("0.1.0"))))
    }

    it ("should delete repos") {
      assert(Manager.repos.delete("testmeta") === Right("Deleted repo testmeta"))
    }
  }
}
