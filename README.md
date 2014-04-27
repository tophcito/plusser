# plusser: A Google+ API Interface for R

The central aim of *plusser* is to facilitate access to the Google+ API for
retrieving posts and profiles from Google's social network. This package is
especially suited to harvest texts and covariate data for social media analysis.

For social media data harvests, this package provides three key functions:

* `harvestPage` to retrieve messages a user posted publicly
* `harvestProfile` to retrieve data from a user's profile (i.e. about the user)
* `harvestActivity` to retrieve the +1ers and resharers of a specific post.

The results are intentionally kept simple for now, so that they can be easily
used in text mining or predictive modeling contexts. There are basic parsing 
functions provided. Users can also use their own parsing functions if required.
At the moment, this package
implements basic authentication (via API keys) only. Full OAuth 2.0
authentication will be implemented later. This will then also be the basis for
functions posting in a user's name.
