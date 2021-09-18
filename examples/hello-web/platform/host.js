const test_decoder = true;

function run() {
  const memory = new Uint8Array(1024);
  const decoder = new TextDecoder();

  function js_display_roc_string(str_bytes, str_len) {
    const utf8_bytes = memory.subarray(str_bytes, str_bytes + str_len);
    const js_string = decoder.decode(utf8_bytes);
    console.log(js_string);
  }

  if (test_decoder) {
    const testAddress = 123;
    const testString = "Hello, world";

    const utf8Encoder = new TextEncoder();
    const targetBytes = memory.subarray(
      testAddress,
      testAddress + testString.length
    );
    const { read, written } = utf8Encoder.encodeInto(testString, targetBytes);
    if (written !== read) {
      throw new Error("Not enough space");
    }

    js_display_roc_string(testAddress, testString.length);
  }
}

run();
