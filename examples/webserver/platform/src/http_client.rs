use roc_app::Metadata;
use roc_std::{RocList, RocStr};
use std::{iter::FromIterator, time::Duration};

pub fn send_req(roc_request: &roc_app::Request) -> roc_app::Response {
    let mut builder = reqwest::blocking::ClientBuilder::new();

    if roc_request.timeout.discriminant()
        == roc_app::discriminant_TimeoutConfig::TimeoutMilliseconds
    {
        let ms: &u64 = unsafe { roc_request.timeout.as_TimeoutMilliseconds() };
        builder = builder.timeout(Duration::from_millis(*ms));
    }

    let client = match builder.build() {
        Ok(c) => c,
        Err(_) => {
            return roc_app::Response::NetworkError; // TLS backend cannot be initialized
        }
    };

    let method = {
        use reqwest::Method;
        use roc_app::Method::*;

        match roc_request.method {
            Connect => Method::CONNECT,
            Delete => Method::DELETE,
            Get => Method::GET,
            Head => Method::HEAD,
            Options => Method::OPTIONS,
            Patch => Method::PATCH,
            Post => Method::POST,
            Put => Method::PUT,
            Trace => Method::TRACE,
        }
    };

    let url = roc_request.url.as_str();

    let mut req_builder = client.request(method, url);
    for header in roc_request.headers.iter() {
        let (name, value) = unsafe { header.as_Header() };
        req_builder = req_builder.header(name.as_str(), value.as_str());
    }
    if roc_request.body.discriminant() == roc_app::discriminant_Body::Body {
        let (mime_type_tag, body_byte_list) = unsafe { roc_request.body.as_Body() };
        let mime_type_str: &RocStr = unsafe { mime_type_tag.as_MimeType() };

        req_builder = req_builder.header("Content-Type", mime_type_str.as_str());
        req_builder = req_builder.body(body_byte_list.as_slice().to_vec());
    }

    let request = match req_builder.build() {
        Ok(req) => req,
        Err(err) => {
            return roc_app::Response::BadRequest(RocStr::from(err.to_string().as_str()));
        }
    };

    match client.execute(request) {
        Ok(response) => {
            let status = response.status();
            let status_str = status.canonical_reason().unwrap_or_else(|| status.as_str());

            let headers_iter = response.headers().iter().map(|(name, value)| {
                roc_app::Header::Header(
                    RocStr::from(name.as_str()),
                    RocStr::from(value.to_str().unwrap_or_default()),
                )
            });

            let metadata = Metadata {
                headers: RocList::from_iter(headers_iter),
                statusText: RocStr::from(status_str),
                url: RocStr::from(url),
                statusCode: status.as_u16(),
            };

            let bytes = response.bytes().unwrap_or_default();
            let body: RocList<u8> = RocList::from_iter(bytes.into_iter());

            if status.is_success() {
                roc_app::Response::GoodStatus(metadata, body)
            } else {
                roc_app::Response::BadStatus(metadata, body)
            }
        }
        Err(err) => {
            if err.is_timeout() {
                roc_app::Response::Timeout
            } else if err.is_request() {
                roc_app::Response::BadRequest(RocStr::from(err.to_string().as_str()))
            } else {
                roc_app::Response::NetworkError
            }
        }
    }
}
