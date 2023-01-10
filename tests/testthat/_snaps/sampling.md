# do_sampling works as expected when case exist

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [76, 87, 229, 342, 353, 150, 13, 413, 24, 237]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 1, 1, 0, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 7, 1, 0, 2, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0, 0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3, 0, 0, 0, 0, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29, 97, 41, 52, 10, 100]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55, 65, 52, 55, 72, 63]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 1, 0, 0, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 10, 10, 10, 10, 10]
        }
      ]
    }

---

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [76, 87, 229, 342, 353]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1]
        }
      ]
    }

# do_sampling works as expected when no cases exist

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [54]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [49]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [10]
        }
      ]
    }

# sample_from_period works as expected

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [179, 216, 261, 320, 264, 216, 69, 103, 170, 341, 136, 377, 497, 468, 80, 487, 18, 1, 34, 497, 487, 18, 339, 34, 468, 29, 105, 227, 226, 332, 80, 377, 69, 24, 427, 195, 369, 226, 103, 264, 103, 237, 466, 195, 237, 105, 321, 466, 103, 103, 377, 427, 158, 108, 237, 466]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 3, 3, 4, 7, 7, 10, 10, 11, 12, 17, 19, 26, 28, 29, 31, 31, 32, 32, 34, 37, 37, 38, 40, 40, 41, 43, 45, 47, 52, 53, 56, 58, 62, 62, 65, 67, 71, 72, 75, 75, 76, 81, 84, 89, 91, 93, 103, 103, 104, 105, 106, 113, 116, 117, 120]
        }
      ]
    }

# sample_from_period works as expected when sample_all_times = FALSE

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [195, 226, 332, 341, 327, 237, 90, 237, 29, 216]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 6, 8, 16, 25, 25, 40, 42, 47, 68]
        }
      ]
    }

# sample_from_period works as expected with multiple proportions

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["sample_id", "id", "for_period", "followup_time", "outcome", "weight", "treatment", "catvarA", "catvarB", "catvarC", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [328, 226, 87, 261, 76, 87, 229, 342, 353, 170, 459, 413, 334, 24, 66, 56, 473, 391, 249, 13, 150, 430, 468, 164, 34, 64, 90, 275, 468, 339]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 6, 8, 16, 16, 16, 16, 18, 19, 19, 25, 26, 29, 29, 40, 40, 40, 40, 41, 42, 47]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 2, 0, 1, 1, 2, 1, 1, 7, 2, 0, 1, 0, 0, 1, 1, 0, 2]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 7, 1, 1, 2, 1, 1, 1, 1, 1, 0, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 0, 0, 0, 1, 0, 1, 2, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 0, 0, 0, 0, 0, 0, 3, 1, 1, 0, 2, 0, 0, 1, 2, 2, 2, 0, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [72, 218, 52, 52, 40, 52, 18, 61, 29, 90, 85, 52, 79, 10, 42, 51, 106, 92, 51, 41, 97, 27, 55, 15, 43, 79, 59, 54, 55, 56]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [68, 58, 55, 61, 44, 55, 62, 46, 55, 88, 58, 55, 65, 72, 61, 57, 64, 62, 68, 52, 65, 35, 34, 57, 76, 81, 62, 59, 34, 61]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 100, 1, 1, 1, 1, 1, 1, 100, 1, 1, 1, 100, 100, 100, 1, 1, 100, 1, 1, 1, 100, 1, 100, 100, 100, 1, 1, 1]
        }
      ]
    }

# case_control_sampling_trials works with separate_files = TRUE

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time", "outcome", "weight", "treatment", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [22, 13, 150, 87, 13, 150, 158, 164, 12, 144, 148, 22, 87, 13, 18, 150, 164, 164, 12, 24, 144, 148, 22, 63, 87, 13, 34, 150, 164, 12]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 262, 262, 262, 262, 262, 262, 262, 262]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 3, 17, 37, 38, 38, 52, 66, 123, 123, 2, 16, 36, 36, 37, 37, 51, 65, 65, 122, 122, 1, 1, 15, 35, 35, 36, 50, 64]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 5, 0, 1, 0, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [165, 45, 40, 43, 45, 40, 32, 14, 21, 64, 32, 169, 43, 49, 113, 42, 14, 14, 22, 18, 74, 32, 177, 30, 46, 50, 35, 48, 14, 23]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [41, 51, 64, 54, 51, 64, 63, 56, 59, 57, 56, 41, 54, 51, 74, 64, 56, 56, 59, 71, 57, 56, 41, 65, 54, 51, 75, 64, 56, 59]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 100, 1, 1, 1, 100, 1, 1, 1, 1, 1, 1, 1, 100, 1, 100, 1, 1, 100, 1, 1, 1, 100, 1, 1, 100, 1, 1, 1]
        }
      ]
    }

# case_control_sampling_trials works with separate_files = FALSE

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["data.frame"]
        },
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["id", "for_period", "followup_time", "outcome", "weight", "treatment", "nvarA", "nvarB", "nvarC", "assigned_treatment", "sample_weight"]
        }
      },
      "value": [
        {
          "type": "integer",
          "attributes": {},
          "value": [22, 13, 150, 87, 13, 150, 158, 164, 12, 144, 148, 22, 87, 13, 18, 150, 164, 164, 12, 24, 144, 148, 22, 63, 87, 13, 34, 150, 164, 12]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 262, 262, 262, 262, 262, 262, 262, 262]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 3, 17, 37, 38, 38, 52, 66, 123, 123, 2, 16, 36, 36, 37, 37, 51, 65, 65, 122, 122, 1, 1, 15, 35, 35, 36, 50, 64]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 5, 0, 1, 0, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [165, 45, 40, 43, 45, 40, 32, 14, 21, 64, 32, 169, 43, 49, 113, 42, 14, 14, 22, 18, 74, 32, 177, 30, 46, 50, 35, 48, 14, 23]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [41, 51, 64, 54, 51, 64, 63, 56, 59, 57, 56, 41, 54, 51, 74, 64, 56, 56, 59, 71, 57, 56, 41, 65, 54, 51, 75, 64, 56, 59]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 100, 1, 1, 1, 100, 1, 1, 1, 1, 1, 1, 1, 100, 1, 100, 1, 1, 100, 1, 1, 1, 100, 1, 1, 100, 1, 1, 1]
        }
      ]
    }

