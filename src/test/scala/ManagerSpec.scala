package adept

import org.scalatest.FunSpec
import semverfi.Version

class ManagerSpec extends FunSpec {
  val testRepo = "git://github.com/softprops/testmeta.git"
  val testRepoName = "testmeta"
  val testModuleName = "semverfi"

  describe("A Manager") {
    it ("should add repos") {
      assert(Manager.repos.add(testRepo) === Right("Cloned repo to %s/%s" format(Config.reposDir, testRepoName)))
    }

    it ("should not same add repo twice") {
      assert(Manager.repos.add(testRepo) === Left("repo %s already added" format testRepo))
    }

    it ("should list repos") {
      assert(Manager.repos.list === List(testRepoName))
    }

    it ("should be updatable") {
      assert(Manager.repos.update() === Right("repos updated"))
    }

    it ("should update repo") {
      assert(Manager.repos.update(Some(testRepoName)) === Right("repos updated"))
    }

    it ("should list modules") {
      val modules = Manager.modules.list
      assert(modules.filter(_._1 == testModuleName) === List((testModuleName, Seq("0.1.0"))))
    }

    it ("should list modules by organization") {
      assert(Manager.modules.byOrganization("me.lessis").headOption.map(_._1) === Some(testModuleName))
    }

    it ("should list modules by name") {
      assert(Manager.modules.byName(testModuleName).headOption.map(_._1) === Some(testModuleName))
    }

    it ("should add modules") {
      val name = "bippy"
      val mod = Module("com.me", name, Version("0.1.0"))
      assert(Manager.modules.add(mod, testRepoName) === Right("ok"))
      assert(Manager.modules.byName(name).headOption.map(_._1) == Some(name))
    }

    it ("should get info about a given module") {
      Manager.modules.info(testModuleName).fold(fail(_), { mod =>
        assert(mod.name === testModuleName)
      })
    }

    it ("should get graph of a given module") {
      val dependentModuleName = "scalacollider"
      Manager.modules.graph(dependentModuleName).fold(fail(_), { mod =>
        assert(mod.name === dependentModuleName)
        assert(mod.dependencies.size === 1)
        mod.dependencies.headOption.map { dep =>
          assert(dep.name === testModuleName)
        }
      })
    }

    it ("should delete repos") {
      assert(Manager.repos.delete(testRepoName) === Right("Deleted repo %s" format testRepoName))
    }
  }
}
