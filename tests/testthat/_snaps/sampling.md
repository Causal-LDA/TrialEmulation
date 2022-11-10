# do_sampling works as expected when case exist

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
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
          "value": [76, 87, 229, 342, 353, 150, 13, 413, 24, 237, 29, 466]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 7, 1, 0, 2, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0, 0, 0, 1, 0, 0, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3, 0, 0, 0, 0, 5, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29, 97, 41, 52, 10, 100, 17, 24]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55, 65, 52, 55, 72, 63, 50, 67]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 10, 10, 10, 10, 10, 10, 10]
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
          "value": [1, 2, 3, 4, 5, 6]
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
          "value": [76, 87, 229, 342, 353, 413]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5, 5, 5, 5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 7, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 2, 0, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 0, 3, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [40, 52, 18, 61, 29, 52]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [44, 55, 62, 46, 55, 55]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 1, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 100]
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
          "value": [1, 2]
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
          "value": [165, 54]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5, 5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7, 7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7, 7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [7, 7]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [70, 49]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [10, 10]
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
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27]
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
          "value": [13, 179, 103, 226, 249, 333, 66, 351, 92, 24, 64, 339, 321, 66, 351, 90, 170, 321, 34, 332, 377, 5, 497, 327, 90, 237, 92]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 4, 5, 6, 8, 16, 18, 19, 25, 26, 29, 40, 41, 42, 47, 49, 54, 59, 68, 69, 93, 96, 98, 102, 107, 109, 124]
        }
      ]
    }

# sample_from_period works as expected when sampling all times

    {
      "type": "list",
      "attributes": {
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125]
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
          "value": [13, 170, 94, 216, 264, 66, 334, 90, 18, 56, 264, 216, 56, 237, 409, 69, 105, 487, 212, 369, 18, 195, 275, 226, 136, 195, 357, 461, 351, 335, 80, 92, 497, 80, 468, 226, 487, 18, 69, 1, 56, 227, 351, 377, 24, 377, 497, 1, 34, 29, 195, 105, 170, 332, 237, 80, 226, 227, 249, 249, 377, 216, 409, 92, 90, 461, 66, 321, 69, 5, 249, 212, 24, 466, 212, 212, 212, 327, 377, 335, 237, 103, 497, 226, 66, 108, 195, 158, 103, 327, 237, 377, 5, 321, 103, 66, 335, 332, 321, 195, 461, 103, 461, 377, 264, 80, 249, 249, 427, 158, 427, 249, 237, 335, 1, 327, 327, 335, 249, 377, 158, 158, 466, 335, 264]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124]
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
          "value": [328, 427, 261, 226, 76, 87, 229, 342, 353, 92, 459, 227, 413, 12, 334, 195, 473, 24, 391, 377, 13, 29, 150, 69, 430, 64, 164, 369, 275, 216]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272, 272]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 8, 8, 16, 16, 18, 18, 19, 19, 25, 25, 26, 26, 29, 29, 40, 40, 41, 41]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 7, 0, 0, 0, 0, 1, 0, 0, 2, 1, 0, 0, 0, 0, 1, 1, 2, 2, 1, 1, 1, 7, 1, 2, 0, 1, 0, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 7, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [2, 7, 0, 0, 1, 0, 1, 2, 0, 0, 1, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 2, 1, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 8, 0, 1, 0, 0, 0, 0, 3, 0, 1, 1, 0, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [72, 148, 52, 218, 40, 52, 18, 61, 29, 29, 85, 48, 52, 7, 79, 47, 106, 10, 92, 8, 41, 17, 97, 16, 27, 79, 15, 70, 54, 35]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [68, 53, 61, 58, 44, 55, 62, 46, 55, 77, 58, 42, 55, 60, 65, 59, 64, 72, 62, 58, 52, 50, 65, 57, 35, 81, 57, 46, 59, 80]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 1, 100, 1, 1, 1, 1, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100]
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
          "value": [22, 150, 87, 13, 13, 148, 150, 158, 164, 101, 12, 144, 144, 148, 103, 22, 94, 87, 13, 13, 127, 150, 127, 164, 136, 12, 34, 144, 148, 101]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 17, 17, 37, 37, 38, 38, 52, 52, 66, 66, 123, 123, 123, 2, 2, 16, 16, 36, 36, 37, 37, 51, 51, 65, 65, 122, 122, 122]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [165, 40, 43, 45, 45, 32, 40, 32, 14, 46, 21, 64, 64, 32, 62, 169, 30, 43, 49, 49, 22, 42, 22, 14, 30, 22, 32, 74, 32, 49]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [41, 64, 54, 51, 51, 56, 64, 63, 56, 51, 59, 57, 57, 56, 71, 41, 59, 54, 51, 51, 44, 64, 44, 56, 47, 59, 75, 57, 56, 51]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 1, 100]
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
          "value": [22, 150, 87, 13, 13, 148, 150, 158, 164, 101, 12, 144, 144, 148, 103, 22, 94, 87, 13, 13, 127, 150, 127, 164, 136, 12, 34, 144, 148, 101]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 260, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261, 261]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [3, 3, 17, 17, 37, 37, 38, 38, 52, 52, 66, 66, 123, 123, 123, 2, 2, 16, 16, 36, 36, 37, 37, 51, 51, 65, 65, 122, 122, 122]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [165, 40, 43, 45, 45, 32, 40, 32, 14, 46, 21, 64, 64, 32, 62, 169, 30, 43, 49, 49, 22, 42, 22, 14, 30, 22, 32, 74, 32, 49]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [41, 64, 54, 51, 51, 56, 64, 63, 56, 51, 59, 57, 57, 56, 71, 41, 59, 54, 51, 51, 44, 64, 44, 56, 47, 59, 75, 57, 56, 51]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 100, 1, 1, 100]
        }
      ]
    }

