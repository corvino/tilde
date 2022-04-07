func pp_json_data(_ data: Data) {
    let obj = try! JSONSerialization.jsonObject(with: data)
    let prettyData = try! JSONSerialization.data(withJSONObject: obj, options: .prettyPrinted)
    let prettyString = String(data: prettyData, encoding: .utf8)!
    print(prettyString)
}

func pp_string_data(_ data: Data) {
    print(String(data: data, encoding: .utf8))
}
