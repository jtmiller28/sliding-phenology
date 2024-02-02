### Proj Notes

#### Rob's take/suggestions
##### Macrophenology is always a question of how do we pull out the signal through lots of noise. 
While the plots do show some signal, the 10 year sliding windows do pose an issue of drowning out the signal through the noise. One big concern when considering this question at the community level is that certain species of plants are making up the bulk of the records, skewing the phenometrics. Some options: <br> 
1. Filter down to a representative species pool, this should be made up of plants that both make up the majority of the records and as well as represent a large span of the vascular plant phylogenetic tree. <br> 
2. Whats really of interest here? The median phenology? We can also think about the local maxima as those peaks may be more interesting at the community level <br>

The way I did the analysis also posses issues when considering the nature of cyclical data, as the current kernel density plots do not address how the phonological distributions should appear when at the 365 - 0 range. Rob recommends a cyclical GAM in order to address this issue. <br> 

Im interested in simulating the data to see: whether the approach itself works and what insights it can give us into global change, what type of data is suited to this method, and when does it break. 

### New updates 02-01-2024
New directions: 
1. Classifying Modality for a given taxa:
   - Challenges: Validation that there was robust enough sampling during the entire phenologic period to qualify modality
2. Looking at how modality may shift given the underlying abiotic variables per year (probably for a small subset of taxa)
3. Append modality strategy as a discrete(?) trait onto the tree to test for phylogenetic signal. 
