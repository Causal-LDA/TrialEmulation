# predict.TE_msm works as expected

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

# predict.TE_msm works with newdata

    {
      "type": "double",
      "attributes": {
        "dim": {
          "type": "integer",
          "attributes": {},
          "value": [5, 9]
        },
        "dimnames": {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "NULL"
            },
            {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "assigned_treatment", "trial_period", "followup_time", "catvarA1", "catvarA2", "catvarA3", "catvarA7", "nvarA"]
            }
          ]
        }
      },
      "value": [-5.86320683, -6.22310539, -6.33457056, -5.83358456, -6.55613605, 0.18631346, 0.53975648, -0.01669477, -0.09900388, -0.08856462, 0.00212081, 0.00335297, 0.00392126, 0.00284737, 0.00468565, 0.00088261, -0.0000148, 0.00284351, -0.00022479, 0.00096501, 0.0198544, 0.05988718, -0.09962968, -0.02303168, 0.11858855, 0.28524271, 0.05743364, -0.13396336, -0.35701359, -0.29294236, -10.40960577, -11.04162648, -12.1003394, -11.28709837, -11.27986276, 0.28675583, 0.54438766, 0.14282542, 0.31030694, 0.34031838, -0.02136835, -0.1229003, -0.05811616, -0.05546671, -0.01289721]
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

# predict.TE_msm works with interactions

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
              "value": [0.01259591, 0.0187253, 0.02171972, 0.02318543, 0.02390355, 0.02425555, 0.02442814, 0.02451276, 0.02455426]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00760414, 0.01155176, 0.01322984, 0.01389294, 0.01418302, 0.01431058, 0.01436698, 0.01439207, 0.0144033]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.01226728, 0.01841097, 0.02152136, 0.02311238, 0.02472089, 0.02686338, 0.02853859, 0.02985207, 0.03088424]
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
              "value": [0.01259591, 0.01259592, 0.01259592, 0.01259592, 0.01259592, 0.01259592, 0.01259592, 0.01259592, 0.01259592]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.00760414, 0.00760414, 0.00760414, 0.00760414, 0.00760414, 0.00760414, 0.00760414, 0.00760414, 0.00760414]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.01226728, 0.01226728, 0.01226728, 0.01226728, 0.01226728, 0.01226728, 0.01226728, 0.01226728, 0.01226728]
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
              "value": [0, -0.00612938, -0.0091238, -0.01058951, -0.01130763, -0.01165964, -0.01183222, -0.01191684, -0.01195834]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0, -0.0062295, -0.01057137, -0.01411454, -0.016864, -0.01900648, -0.02068169, -0.02199517, -0.02302734]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0, -0.00351999, -0.00504363, -0.00570673, -0.00599681, -0.00612437, -0.00618078, -0.00620587, -0.00621709]
            }
          ]
        }
      ]
    }

