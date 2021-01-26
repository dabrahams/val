import Foundation
import Driver

let arguments = try ArgumentParser(CommandLine.arguments)

var driver = Driver()
driver.context.diagnosticConsumer = Terminal(sourceManager: driver.context.sourceManager)
driver.jobs.append(CompileJob.stdlib(path: arguments.sysroot))
try driver.run()