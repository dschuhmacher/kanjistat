# print kanjivec object

    Code
      print(kanjivec_ref_fuji)
    Output
      Kanjivec representation of 藤 (Unicode 85e4)
      18 stroke vector graphics with depth 4 decomposition

# str for kanjivec object

    Code
      str(kanjivec_ref_fuji)
    Output
      'kanjivec':	11 components:
       $ char      : chr "藤"
       $ hex       : 'hexmode' int 85e4
       $ padhex    : chr "085e4"
       $ family    : chr "schoolbook"
       $ nstrokes  : int 18
       $ ncompos   : num [1:5] 1 2 3 4 6
       $ nveins    : int 6
       $ strokedend:  ..--[dendrogram w/ 2 branches and 18 members at h = 5, label = 藤]
        ..  |--[dendrogram w/ 3 branches and 3 members at h = 4, label = 艹]
        ..  |  |--leaf "㇐" (h= 3  )
        ..  |  |--leaf "㇑a" (h= 3  )
        ..  |  `--leaf "㇑a" (h= 3  )
        ..  `--[dendrogram w/ 2 branches and 15 members at h = 4, label = 滕]
        ..     |--[dendrogram w/ 4 branches and 4 members at h = 3, label = 月]
        ..     |  |--leaf "㇓" (h= 2  )
        ..     |  |--leaf "㇆a" (h= 2  )
        ..     |  |--leaf "㇐a" (h= 2  )
        ..     |  `--leaf "㇐a" (h= 2  )
        ..     `--[dendrogram w/ 2 branches and 11 members at h = 3, label = 劵]
        ..        |--[dendrogram w/ 4 branches and 6 members at h = 2, label = g5]
        ..        |  |--leaf "㇔" (h= 1  )
        ..        |  |--leaf "㇒" (h= 1  )
        ..        |  |--[dendrogram w/ 2 branches and 2 members at h = 1, label = 二]
        ..        |  |  |--leaf "㇐" 
        ..        |  |  `--leaf "㇐" 
        ..        |  `--[dendrogram w/ 2 branches and 2 members at h = 1, label = 人]
        ..        |     |--leaf "㇒" 
        ..        |     `--leaf "㇏" 
        ..        `--[dendrogram w/ 5 branches and 5 members at h = 2, label = 氺]
        ..           |--leaf "㇚" (h= 1  )
        ..           |--leaf "㇔" (h= 1  )
        ..           |--leaf "㇀" (h= 1  )
        ..           |--leaf "㇒" (h= 1  )
        ..           `--leaf "㇔/㇏" (h= 1  )
       $ components: (list of components organized by level)
       $ veins: (list of component coordinates from root to last inner node)
       $ stroketree: (nested list with original kanjivg info)

