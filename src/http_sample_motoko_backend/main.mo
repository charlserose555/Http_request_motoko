import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Prim "mo:⛔";
import Prelude "mo:base/Prelude";
import Float "mo:base/Float";
import Nat64 "mo:base/Nat64";
import Debug "mo:base/Debug";
import Char "mo:base/Char";
import Nat32 "mo:base/Nat32";
import Int64 "mo:base/Int64";
import Error "mo:base/Error";

actor HttpRequestSample {
  type HeaderField = (Text, Text);

  type HttpResponse = {
    status_code: Nat16;
    headers: [HeaderField];
    body: Blob;
    upgrade: ?Bool;
  };

  type HttpRequest = {
    method: Text;
    url: Text;
    headers: [HeaderField];
    body: Blob;
  };

  public query func http_request(req : HttpRequest) : async HttpResponse {
    Debug.print("result: " # debug_show(Text.decodeUtf8(req.body)));

    let input = Text.decodeUtf8(req.body);

    let value : Float = textToFloat(input);

    let doubleValue = Float.mul(value, 2);

    switch (req.method, req.url) {
      case ("GET", "/") {{
        status_code = 200;
        headers = [ ("content-type", "text/plain") ];
        body = Text.encodeUtf8(Float.toText(doubleValue));
        upgrade = ?false;
      }};

      case ("POST", "/") {{
        status_code = 204;
        headers = [];
        body = "";
        streaming_strategy = null;
        upgrade = ?true;
      }};
      case _ {{
        status_code = 400;
        headers = [];
        body = "Invalid request";
        streaming_strategy = null;
        upgrade = null;
      }};
    }
  };

  private func textToFloat(t : ?Text) : Float {

    var i : Float = 1;
    var f : Float = 0;
    var isDecimal : Bool = false;

    switch(t) {
      case(null) { 0 };
      case(?_t) { 
        for (c in _t.chars()) {
          if (Char.isDigit(c)) {
            let charToNat : Nat64 = Nat64.fromNat(Nat32.toNat(Char.toNat32(c) -48));
            let natToFloat : Float = Float.fromInt64(Int64.fromNat64(charToNat));
            if (isDecimal) {
              let n : Float = natToFloat / Float.pow(10, i);
              f := f + n;
            } else {
              f := f * 10 + natToFloat;
            };
            i := i + 1;
          } else {
            if (Char.equal(c, '.') or Char.equal(c, ',')) {
              f := f / Float.pow(10, i); // Force decimal
              f := f * Float.pow(10, i); // Correction
              isDecimal := true;
              i := 1;
            } else {
              return f;
            };
          };
        };
      return f;
    } 
    };     
  };

};
