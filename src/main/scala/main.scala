package adept

object Main {
  def main(args: Array[String]): Unit = {
    println(Module.read("""{
      "name":"foo",
      "organization":"my.comp",
      "version":"0.1.0",
      "dependencies":[{
      "name":"foo",                                                                                                                                          
      "organization":"my.comp",                                                                                                                              
      "version":"0.1.0"}]
    }"""))
  }
}
