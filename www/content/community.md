# Community

[Roc Zulip Chat](https://roc.zulipchat.com/) is the most active community gathering place.
We love it when new people stop by and introduce themselves in [`#introductions`](https://roc.zulipchat.com/#narrow/stream/387892-introductions) so others can welcome you to the community!

The Roc Online Meetup meets about once per month. The organizer posts a [when2meet](https://when2meet.com) for the next month's meetup in [`#gatherings` on Zulip](https://roc.zulipchat.com/#narrow/stream/303057-gatherings), so we can find the best time that works for everyone
who's interested in attending. The organizer then posts a finalized time and a link to where people can join. They usually last between 1-2 hours, and consist of some casual show-and-tell presentations followed by conversation. They're fun!

We have not had a Roc conference yet, although there have been a few Roc talks given at conferences,
and a few times when Roc community members have met up in person.

### [Code of Conduct](#code-of-conduct) {#code-of-conduct}

The Roc project enforces [a code of conduct](https://github.com/roc-lang/roc/blob/main/code_of_conduct.md). Please read and follow it!

### [Contributing](#contributing) {#contributing}

All the source code to the Roc project is on GitHub in the [roc-lang](https://github.com/roc-lang) organization. The compiler and CLI are at [roc-lang/roc](https://github.com/roc-lang/roc), and there's a tag for [Good First Issues](https://github.com/roc-lang/roc/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22).

The project has [many contributors](https://github.com/roc-lang/roc/graphs/contributors), and we like to invest in helping new contributors get started. If you'd like to become a new contributor (even if you don't know what you'd like that contribution to be yet), just make a post in [the "new contributors" topic](https://roc.zulipchat.com/#narrow/stream/316715-contributing/topic/new.20contributors) and say hello!

### [Ideas, proposals, and feature requests](#ideas) {#ideas}

Roc doesn't have a formal process for managing design proposals.

At the current size of the project, having a formal process (like a [RFC](https://en.wikipedia.org/wiki/Change_request) system) would be more heavyweight than it's worth. For now, the guiding principle is that as a community we should all be friendly, supportive, and openly share and discuss ideas without the expectation that they will necessarily be accepted or not. We follow a [BDFN](/bdfn) leadership model today, although this is planned to change someday.

There are three loose stages that a design proposal can go through in Roc: idea, proposal, and implementation. These are guidelines, not strict requirements; their goal is to prevent the outcome where someone does a lot of implementation work only to have their contribution never make it into the code base because it's determined that we wanted to go in a different design direction. Confirming ahead of time that the design direction is desired can prevent implementing something that ends up not being used.

In the idea stage, people are encouraged to describe their idea and explore the problem, potential solutions, and tradeoffs. It's a good idea to share the idea in [`#ideas` on Zulip](https://roc.zulipchat.com/#narrow/stream/304641-ideas). There's no prerequisite for sharing an idea (it's only an idea, after all!) and likewise there's also no obligation for any contributor to necessarily act on it.

If the idea seems promising and worth developing further (as confirmed by a Roc contributor with expertise in the relevant area—not necessarily the [BDFN](/bdfn)), usually the next step is to get more specific with a written proposal that details all the necessary information about what the change would involve.

A written proposal isn't always necessary (for example, it may be deemed a simple and uncontroversial enough change that we're comfortable proceeding straight to implementation), but since writing proposals can be time-consuming, it's definitely a good idea to get confirmation at the idea stage from an experienced contributor before taking the time to write one up.

There's no guarantee that a proposal will be accepted, and even if it is, there's no guarantee that something won't come up during implementation that changes the tradeoffs and means it doesn't end up making it into the language after all. But if it is accepted (again, doesn't have to be by the [BDFN](/bdfn) - although the BDFN does have final say if there's a disagreement at some point), that means a [Pull Request](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request) should be reviewed, and the general understanding is that the PR will be accepted unless some problem is discovered during implementation that wasn't anticipated at the proposal stage.

This is the process we're using for now, but of course it may change in the future!
