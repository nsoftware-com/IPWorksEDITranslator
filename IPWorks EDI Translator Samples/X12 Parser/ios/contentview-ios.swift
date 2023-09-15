import SwiftUI
import IPWorksEDITranslator

struct ContentView: View, X12ReaderDelegate {
  func onEndFunctionalGroup(tag: String, controlNumber: String, count: Int32, fullSegment: String) {
    outputRes+="EndFunctionalGroup \(tag)\n"
  }
  func onEndInterchange(tag: String, controlNumber: String, fullSegment: String) {
    outputRes+="EndInterchange \(tag)\n"
  }
  func onEndLoop() {
    outputRes+="EndLoop\n"
  }
  func onEndTransaction(tag: String, controlNumber: String, count: Int32, fullSegment: String) {
    outputRes+="EndTransaction \(tag)\n"
  }
  func onError(errorCode: Int32, description: String) {
    outputRes+="ERROR: \(errorCode): \(description)\n"
  }
  func onResolveSchema(transactionCode: String, standardVersion: String) {
    outputRes+="ResolveSchema \(transactionCode)\n"
  }
  func onSegment(tag: String, name: String, loopName: String, fullSegment: String) {
    outputRes+="Segment \(name)\n"
  }
  func onStartFunctionalGroup(tag: String, controlNumber: String, fullSegment: String) {
    outputRes+="StartFunctionalGroup \(tag)\n"
  }
  func onStartInterchange(tag: String, controlNumber: String, fullSegment: String) {
    outputRes+="StartInterchange \(tag)\n"
  }
  func onStartLoop(name: String) {
    outputRes+="StartLoop \(name)\n"
  }
  func onStartTransaction(tag: String, controlNumber: String, code: String, fullSegment: String) {
    outputRes+="StartTransaction \(tag)\n"
  }
  func onWarning(warnCode: Int32, message: String, segmentNumber: Int32, segmentTag: String, technicalErrorCode: String, segmentErrorCode: String, elementErrorCode: String, elementPosition: Int32) {
    outputRes+="WARNING \(warnCode): \(message)"
  }
  
  var reader = X12Reader()
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  var formats = ["json","auto"]
  @State private var selectedFormat: String = "auto"
  @State private var schema: String = "/Applications/IPWorks EDITranslator 2022 macOS Edition/demos/X12 Parser/RSSBus_00401_810.json"
  @State private var filename: String = "/Applications/IPWorks EDITranslator 2022 macOS Edition/demos/X12 Parser/x12.txt"
  @State private var outputRes: String = ""
  
  var body: some View {
    VStack(alignment: .leading)
    {
      Text("This demo shows how to parse EDI data with the X12Reader module.").foregroundColor(Color.blue)
      HStack{
        Text("Schema file:")
        TextField("Enter input filepath", text: $schema)
      }
      Picker("Select a schema format",selection: $selectedFormat) {
        ForEach(formats, id: \.self){
          Text($0)
        }
      }.frame(maxHeight: 60).padding(.vertical, 10)
      
      Group
      {
        HStack{
          Text("Input File:")
          TextField("Enter filepath", text: $filename)
        }
        parseButton()
        
        Text("Output:")
        TextEditor(text: $outputRes)
          .border(Color.black, width: 1)
      }
    }.padding(.all, 5.0)
  }
  
  @ViewBuilder
  private func parseButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      reader.delegate = self
      outputRes = ""
      do
      {
        switch selectedFormat {
        case "json":
          reader.schemaFormat = X12readerSchemaFormats.schemaJSON
        default:
          reader.schemaFormat = X12readerSchemaFormats.schemaAutomatic
        }
        try reader.loadSchema(fileName: schema)
        reader.inputFile = filename
        try reader.parse()
      }
      catch
      {
        outputRes += "Error: \(error)"
        return
      }
    }, label: {
      Text("Parse")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    })
    .buttonStyle(PlainButtonStyle())
  }
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}

