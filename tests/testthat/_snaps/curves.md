# predict.RTE_model works as expected

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.0046817, 0.00933138, 0.01394965, 0.01853709, 0.02309424, 0.02762164]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00358595, 0.00715416, 0.0107049, 0.01423843, 0.01775497, 0.02125478]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00109575, -0.00217722, -0.00324475, -0.00429866, -0.00533927, -0.00636686]
            }
          ]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.0046817, 0.00933138, 0.01394965, 0.01853709, 0.02309424, 0.02762164]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00358595, 0.00715416, 0.0107049, 0.01423843, 0.01775497, 0.02125478]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00109575, -0.00217722, -0.00324475, -0.00429866, -0.00533927, -0.00636686]
            }
          ]
        }
      ]
    }

# predict.RTE_model works with newdata

    [
      [-5.8632, 0.1863, 0.0021, 0.0009, 0.0199, 0.2852, -10.4096, 0.2868, -0.0214],
      [-6.2231, 0.5398, 0.0034, -0, 0.0599, 0.0574, -11.0416, 0.5444, -0.1229],
      [-6.3346, -0.0167, 0.0039, 0.0028, -0.0996, -0.134, -12.1003, 0.1428, -0.0581],
      [-5.8336, -0.099, 0.0028, -0.0002, -0.023, -0.357, -11.2871, 0.3103, -0.0555],
      [-6.5561, -0.0886, 0.0047, 0.001, 0.1186, -0.2929, -11.2799, 0.3403, -0.0129]
    ]

---

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["assigned_treatment_0", "assigned_treatment_1", "difference"]
        }
      },
      "value": [
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00543004, 0.01083993, 0.01622967, 0.0215993, 0.02694883, 0.03227829, 0.03758771, 0.0428771, 0.04814648]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00493959, 0.0098556, 0.01474814, 0.01961731, 0.02446323, 0.02928601, 0.03408575, 0.03886255, 0.04361654]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00613676, 0.01223465, 0.01829394, 0.02431488, 0.03029772, 0.03624271, 0.04215009, 0.04802012, 0.05385304]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00498511, 0.00995399, 0.01490662, 0.01984303, 0.02476322, 0.02966719, 0.03455497, 0.03942656, 0.04428196]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00499679, 0.00997788, 0.01494327, 0.01989298, 0.02482699, 0.02974531, 0.03464794, 0.03953489, 0.04440616]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00815372, 0.01623854, 0.02425507, 0.03220388, 0.04008558, 0.04790073, 0.05564993, 0.06333373, 0.07095271]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["followup_time", "cum_inc_diff", "2.5%", "97.5%"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            },
            "row.names": {
              "type": "integer",
              "attributes": {},
              "value": [1, 2, 3, 4, 5, 6, 7, 8, 9]
            }
          },
          "value": [
            {
              "type": "integer",
              "attributes": {},
              "value": [0, 1, 2, 3, 4, 5, 6, 7, 8]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.00044493, -0.00088594, -0.00132305, -0.00175627, -0.00218561, -0.0026111, -0.00303273, -0.00345054, -0.00386452]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [-0.0005684, -0.00112998, -0.00168478, -0.00223288, -0.00277433, -0.0033092, -0.00383756, -0.00435945, -0.00487495]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00318353, 0.0063233, 0.00941976, 0.0124734, 0.01548467, 0.01845402, 0.02138191, 0.02426879, 0.02711509]
            }
          ]
        }
      ]
    }

