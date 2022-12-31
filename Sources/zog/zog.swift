import Foundation

public var globalParameters = (
    showTypes: false,
    showUnification: false
)

@main
public struct zog {
    public static func main() {
        if CommandLine.arguments.count > 1 {
            do {
                let sourceFile = CommandLine.arguments[1]
                globalParameters.showTypes = CommandLine.arguments.contains("--show-types")
                let resolver = Resolver() 
                _ = try resolver.resolve(path: sourceFile, level: 0)
            } catch let error as ParserError {
                print("Parsing failed: \(error)")
            } catch let error as TypeError {
                print("Type inference failed: \(error)")
            } catch let error {
                print("Error: \(error)")
            }
        } else {
            print("usage: zog file.zog")
        }
    }
}
