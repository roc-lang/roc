
# Crash Reporting

## Background

Sometimes the roc compiler crashes. Other compilers, like Rust, automatically suggest opening an issue for when this happens. We could reduce friction and make it so that you can report the problem right away from the command line, without having to switch contexts to your browser and open an issue.

Some systems proactively prompt you to enable background diagnostic reporting. I don't like these prompts, and I don't want the Roc compiler to have one. However, I do like the idea of making it easy for someone to opt into a setting like "automatically send bug reports whenever the compiler crashes" if they want to—just so long as we don't proactively ask them to enable it before a crash has actually happened.

Here's a proposal for how to achieve those goals.

## Proposal

Whenever the Roc compiler crashes (we'll have to change over a lot of macros to do this, but that's a good idea regardless), we print a message like this:

<pre><samp>
The Roc compiler crashed! You can automatically report this to our public crash tracker by re-running this command with the environment variable ROC_SEND_CRASH_REPORTS set to either WITH_SOURCES (to include your .roc files, which can help reproduce the crash) or WITHOUT_SOURCES to exclude them. A quick way to do this is to run one of these at the command line:

ROC_REPORT_CRASHES=WITH_SOURCES !!

Or, if you would prefer that the report not include your source files:

ROC_REPORT_CRASHES=WITHOUT_SOURCES !!

Everything in these crash reports can be viewed publicly, so make sure you only use the WITH_SOURCES option if you're okay with your source files being public! Either way, the report will always include:
    - Your operating system ($OS_GOES_HERE)
    - The version of Roc you're running ($VERSION_GOES_HERE)
    - The compiler error, along with a backtrace of the compiler's call stack

You can also manually file a bug report about this at https://github.com/roc-lang/roc/issues if you prefer. Either way, more information will help the people making Roc fix crashes like this in future releases!
</pre></samp>

Naturally, if someone always wants to report crashes, they can set the environment variable globally.

## Implementation Notes

On a technical level, the way to automate gathering up all the relevant files at the point of the crash would be:

1. When beginning a build, set a global variable to record the filename of the root module being built.
2. If a crash happens, go look up that filename and immediately find all the files that one references. (If this procedure also crashes, we're out of luck! It's conceivable that we could make that whole code path panic-free, since the only thing we should need to do there is parsing.)
3. Include the contents of those source files in the report, along with their filenames. (We don't include platform binaries because those would take a potentially long time to upload and take up a lot of space in the crash reporter, but in practice, 99% of the time the source files will likely include a URL to the platform being used anyway.)

As far as how to receive these files, the idea would be to set up an upload endpoint that we can rely on to have essentially 100% uptime without needing to maintain it. For example, DriveHQ offers a [free FTP endpoint](https://www.drivehq.com/help/Price/Cloud_IT_Service_Pricing_Personal.aspx) for up to 5,000 files and 5GB, or for $4/month, up to 64GB and 50,000 files - which I'd consider an acceptable cost for this. We could use this essentially as a staging area: periodically go through and triage reports, turn them into GitHub issues and copy over the files to reproduce (possibly moving them somewhere longer-term, e.g. a free Dropbox account). People know to expect that the contents of the report might end up in a public GitHub issue because we set that expectation in the original message.

The nice thing about a FTP endpoint is that FTP is just a protocol, so we can switch to a different provider (including hosting our own endpoint) in the future if we want to, without breaking all the existing roc clients out there. (We can host a static file on roc-lang.org which tells us what FTP domain to upload to, e.g. have roc-lang.org/ftp-domain.txt be ftp.drivehq.com/roc for example.) In contrast, if we had the uploads use a specific service's API like S3 or Dropbox, it would probably not be possible to seamlessly change backends in the future.