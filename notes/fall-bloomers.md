### Fall Bloomers notes:

Of particular interest is the wide shift we see among the fall blooming taxa within the Sonoran Desert. These havent been discussed in the literature nearly as much as the spring shifts, but could represent important phenological shifts to impact desert ecology. 

To assess these trends: <br>
1) We need to subset the data to represent taxa that have sufficient blooming records within the Fall. <br>
	Keeping in mind: <br>
		a. What is enough to be classified as a taxa that reliably blooms in the fall? <br>
			- Take the taxa with >3 records within the fall and compare time bins (simply do a test that asks whether they are blooming consistently in the fall or not. <br>
		b. Enough data...5 taxa was our cut off. We could choose to do what we did last time and assess the community level trends, however; this may miss out on all of the underlying (and interesting) biology. We should probably try to find some well represented indicator species across (most) time bins to build the analysis. <br>
		c. What is fall? There are probably some interesting ways to assess this, however; After looking at the initial trends plotted a simple anwser could be that its anything =>200 doy. <br>

2) Once we have curated a dataset that represents only the fall bloomers, run the sliding window analysis on the phenology of the plants using kde and phenesse. Focus using Phenesse if there is adequate species representation. <br>
	Issues: fall bloomers *may* be pushed into the winter months (so late that they could appear to be early spring bloomers). This also has implicaitions of how kde (and possibly phenesse) will assess these trends. <br>
