---
output: 
  html_document: 
    keep_md: yes
---



<style type="text/css">
body, td {
   font-size: 16px;
}
.table { width: auto; }
</style>



# About the Database

The "DFMorph" (Darwin's Finch Morphology) database contains data, mostly morphological (e.g., beak dimensions), on individual Darwin’s finches. The data are from a number of published and unpublished sources.

Additional information includes the publication, source of the specimen, the original specific or subspecific name
assigned by the author(s), museum location and catalog number, and other information of historical importance.
In particular, Sulloway's (1982) measurements of existing Beagle specimens have been included.

This version contains data from four sources. The first three were in BIRDD version 1, the last is new with
version 2.

 * The largest published collection of measurements, taken from Snodgrass & Heller's important early work
(1904).
 * Sulloway's historically significant examination of the history of all surviving finch specimens collected by
members of the Beagle expedition (1982).
 * Measurements published in Swarth's (1931) monograph.
 * Over 6,500 specimens that David Lack measured for his 1945 monograph and 1947 book. Lack deposited copies
of his measurements in the British Museum (Natural History) and the California Academy of Sciences (CAS);
we optioned copies of the latter from the CAS Archives. These more extensive data provide many opportunities
for students to explore variation among individual finches and island populations, and to try to understand how
taxonomists separate specimens into species and subspecies.

There is considerable overlap among these subsets of data. Some specimens, such as those of the Beagle
expedition, may appear in all of them. The Dataset Choice field on the Table of Contents allows you to determine
which set of data to examine and export.

# Bibliography

Lack, D.L. (1945). “The Galápagos finches (Geospizinae): a study in variation.” Occasional Papers of the
California Academy of Sciences 21: 1-159.

Lack, D.L. (1947). Darwin's Finches: an essay on the general biological theory of evolution. Cambridge, England,
Cambridge University Press.

Lack, D.L. (1969). “Subspecies and sympatry in Darwin's Finches.” Evolution 23: 252-263.

Snodgrass, R. E. and E. Heller (1904). “Papers from the Hopkins-Stanford Galapagos Expedition, 1898-99 XVI.
Birds.” Proc. Wash. Acad. Sci. 5: 231-372.

Sulloway, F. J. (1982). “The Beagle collections of Darwin's Finches (Geospizinae).” Bulletin of the British
Museum (Natural History), Zoology series 43: 49-94.

Swarth, H. S. (1931). “The avifauna of the Galapagos Islands.” Occ. Pap. Calif. Acad. Sci 18: 5-299.

# Notes About the Data

## Taxon Names and IDs

The initial confusion and debate over the classification of Darwin's finches settled down in 1931 with Swarth's
monograph. Lack made relatively minor changes (1945, 1947, 1969) and with some exceptions, that is the system
used by most modern workers. We have elected to use Lack's 1969 classification as our standard. Lack's names for
genus and species names plus subspecies if appropriate are the "modern" taxa and are listed with ".L69" at the end
of the field name. In addition, wherever possible we have also recorded the original author's name. See references
and notes in the `Species` database for more details.

Some of the taxonomic names are lengthy, so we also have a more compact `Species.ID` as well as a `Subspecies.ID`
where necessary. The `Species.ID` is made up of the first 3 letters of Lack's genus, a period, and the first 3 letters of
Lack's species name. For subspecies, a period and the first 3 letters of Lack's subspecies name are added.

For example, the specimens of sharp beaked ground finches on Darwin/Culpepper have the following values:

**Field Name** | **Example Taxon**
-------------- | -----------------
`TaxonOrig` | _Geospiza septentrionalis_
`TaxonL69`      | _Geospiza difficilis septentrionalis_
`GenusL69`      | _Geospiza_
`SpeciesL69`    | _difficilis_
`SubspL69` | _septentrionalis_
`SpeciesID`     | _Geo.dif_
`SubspID`  | _Geo.dif.sep_

## Notes on Data Fields

The terms “beak” and “bill” are used interchangeably.

Here are notes on commonly used data and measurements. Specific definitions and procedures for that source's data are described later. The following picture
summarizes the various measurements.

![Morphology Measurements](https://qubeshub.org/groups/birdd/File:/uploads/DFMORPH_interpret.gif)
