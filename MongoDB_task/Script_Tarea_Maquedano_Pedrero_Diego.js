////EJERCICIOS TAREA MONGODB\\\\

//1.
db.movies.find()

//2.
db.movies.count()

//3.
db.movies.insert({title : "Peli_prueba", year : 2000, cast : [ ],genres : [ ]})

//4.
db.movies.deleteOne({title:"Peli_prueba"})

//5.
db.movies.count({cast:"and"})

//6.
db.movies.updateMany({}, 
    {$pull:{cast:"and"}}
)

//7.
db.movies.count({cast:[]})

//8.
db.movies.updateMany({cast:[]}, 
    {$push: {cast:"Undefined"}}
)
db.movies.find({cast:"Undefined"})

//9.
db.movies.count({genres:[]})

//10.
db.movies.updateMany({genres:[]}, 
    {$push: {genres:"Undefined"}}
)
db.movies.find({genres:"Undefined"})

//11.
db.movies.aggregate([
    {
        $group: {
            _id:null,
            year:{$max: "$year"}
        }
    }
])

//12. Total desglosado por año
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $sort: {"_id":-1}
    },
    {
        $limit: 20
    },
    {
        $project: {_id:"$_id",total:{$size: "$pelis"}} 
    }
])

//12. Total general
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $sort: {"_id":-1}
    },
    {
        $limit: 20
    },
    {
        $unwind: "$pelis"
    },
    {
        $group: { _id: null,
            total:{$sum: 1}
        }
    }
])

//13. 
db.movies.aggregate([
    {
       $match: {year:{$gte: 1960, $lte:1969}} 
    },
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $unwind: "$pelis"
    },
    {
        $group: { _id: null,
            total:{$sum: 1}
        }
    }
])

//14. Query sin sort
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $project: {_id:"$_id",total:{$size: "$pelis"}} 
    },
    {
        $group: { _id: null,
            max:{$max: "$total"},
            items:{$push: {year:"$_id",total:"$total"}}
        }
    },
    {
        $unwind: "$items"
    },
    {
        $match:{$expr:{$eq:["$max", "$items.total"]}}
    },
    {
        $project: {year:"$items.year",pelis:"$items.total"}
    }
])

//14. Query con sort
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $project: {_id:"$_id",total:{$size: "$pelis"}} 
    },
    {
        $sort: {total:-1}
    },
    {
        $limit: 3
    }
])

//15. Query sin sort
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $project: {_id:"$_id",total:{$size: "$pelis"}} 
    },
    {
        $group: { _id: null,
            min:{$min: "$total"},
            items:{$push: {year:"$_id",total:"$total"}}
        }
    },
    {
        $unwind: "$items"
    },
    {
        $match:{$expr:{$eq:["$min", "$items.total"]}}
    },
    {
        $project: {year:"$items.year",pelis:"$items.total"}
    }
])

//15. Query con sort
db.movies.aggregate([
    {
        $group: {
            _id:"$year",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $project: {_id:"$_id",total:{$size: "$pelis"}} 
    },
    {
        $sort: {total:1}
    },
    {
        $limit: 4
    }
])

//16.
db.movies.aggregate([
    {
        $unwind: "$cast"
    },
    {
        $project: {"_id":0}
    },
    {
        $out: "actors"
    }
])
db.actors.count()

//17.
db.actors.aggregate([
    {
        $match: {cast:{$ne: "Undefined"}}
    },
    {
        $group: {
            _id:"$cast",
            pelis:{$push: {title:"$title"}}
        }
    },
    {
        $project: {_id:"$_id",cuenta:{$size: "$pelis"}} 
    },
    {
        $sort: {cuenta:-1}
    },
    {
        $limit: 5
    }
])

//18:
db.actors.aggregate([
    {
        $group: {
            _id:{title:"$title",year:"$year"},
            actores:{$push: {nombre:"$cast"}}
        }
    },
    {
        $project: {_id:"$_id",cuenta:{$size: "$actores"}} 
    },
    {
        $sort: {cuenta:-1}
    },
    {
        $limit: 5
    }
])

//19.
db.actors.aggregate([
    {
        $match: {cast:{$ne: "Undefined"}}
    },
    {
        $group: {
            _id:"$cast",
            comienza:{$min:"$year"},
            termina:{$max:"$year"},
        }
    },
    {
        $addFields:{anos: { $subtract: ["$termina", "$comienza"] }}
    },
    {
        $sort: {anos:-1}
    },
    {
        $limit: 5
    }
])

//20.
db.actors.aggregate([
    {
        $unwind: "$genres"
    },
    {
        $project: {"_id":0}
    },
    {
        $out: "genres"
    }
])
db.genres.count()

//21. 
db.genres.aggregate([
    {
        $group: {
            _id:{year:"$year",genre:"$genres"},
            distinct_pelis:{$addToSet: "$title"}
        }
    },
    {
        $project: {_id:"$_id",pelis:{$size: "$distinct_pelis"}}
    },
    {
        $sort: {pelis:-1}
    },
    {
        $limit: 5
    }
])

//22.
db.genres.aggregate([
    {
        $match: {cast:{$ne: "Undefined"}}
    },
    {
        $group: {
            _id: "$cast",
            
            distinct_generos:{$addToSet: "$genres"}
        }
    },
    {
        $project: {_id:"$_id",numgeneros:{$size: "$distinct_generos"},generos:"$distinct_generos"}
    },
    {
        $sort: {numgeneros:-1}
    },
    {
        $limit: 5
    }
])

//23.
db.genres.aggregate([
    {
        $group: {
            _id:{title:"$title", year:"$year"},
            distinct_generos:{$addToSet: "$genres"}
        }
    },
    {
        $project: {_id:"$_id",numgeneros:{$size: "$distinct_generos"},generos:"$distinct_generos"}
    },
    {
        $sort: {numgeneros:-1}
    },
    {
        $limit: 5
    }
])


/////////////////////////
/////////////////////////

//24.
db.genres.aggregate([
    {
        $match: {title:{$regex:"^The"}, genres:{$ne: "Undefined"}}
    },
    {
        $group: {
            _id:"$genres",
            distinct_pelis: {$addToSet: "$title"}
        }
    },
    {
        $project: {total_pelis:{$size: "$distinct_pelis"}}
    }
])

//25.
db.genres.aggregate([
    {
        $match: {genres: "Mystery"}
    },
    {
        $group: {
            _id: "$year",
            distinct_actores: {$addToSet: "$cast"}
        }
    },
    {
        $project: {cuenta:{$size: "$distinct_actores"},actores:"$distinct_actores"}
    },
    {
        $sort: {cuenta:-1}
    },
    {
        $limit: 5
    }
])

//26.
 db.movies.aggregate([
     {
        $project: {_id:"$_id",
                 title:"$title",
                 year:"$year",
                 generos:"$genres",
                 total_actores: {$size: "$cast"},
                }
     },
     {
         $match: {$and:[{total_actores:{$gte: 3}},{total_actores:{$lte: 5}}], generos:"Undefined"}
         
     },
     {
         $group: { _id: "$year",
                   cuenta:{$sum: 1},
                   titulos:{$addToSet: "$title"}
             
         }
     },
     { $sort : { cuenta : -1 } },
     { $limit:  5 } 
])

