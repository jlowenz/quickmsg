#pragma once

#include <json_spirit.h>

namespace quickmsg {

  using namespace json_spirit;
  
  class AddNInts
  {
  public:
    std::string create_req(const std::vector<int>& add_ints);
    std::string create_resp(const std::string& req);
    std::vector<int> vals_from_req(const std::string& req);
    int result_from_resp(const std::string& resp);
    std::vector<int> mArray_to_vec(const mArray& m_arr);
  };

  std::vector<int> mArr_to_vec(const mArray& m_arr)
  {
    std::vector<int> out;
    for (auto i=0; i<m_arr.size(); ++i)
    {
      out.push_back(m_arr[i].get_value<int>());
    }
    return out;
  }

  std::string AddNInts::create_req(const std::vector<int>& add_ints)
  {
    mObject req_fields; // use std map type
    mValue val(add_ints.begin(), add_ints.end());
    req_fields["ints_to_add"] = val;
    return write(req_fields, pretty_print);
  }

  std::vector<int> AddNInts::vals_from_req(const std::string& req)
  {
    mValue value;
    read(req, value);
    mObject& req_fields = value.get_obj();
    mArray ints_m_arr = req_fields["ints_to_add"].get_value<mArray>();
    std::vector<int> ints_to_add = mArr_to_vec(ints_m_arr);
    return ints_to_add;
  }

  std::string AddNInts::create_resp(const std::string& req)
  {
    mValue value;
    read(req, value);
    mObject& req_fields = value.get_obj();
    mArray ints_m_arr = req_fields["ints_to_add"].get_value<mArray>();
    std::vector<int> ints_to_add = mArr_to_vec(ints_m_arr);
    int result = std::accumulate(ints_to_add.begin(), ints_to_add.end(), 0);
    
    mObject resp_fields;
    mValue val(ints_to_add.begin(), ints_to_add.end());
    resp_fields["ints_to_add"] = val;
    resp_fields["result"] = result;
    return write(resp_fields, pretty_print);
  }

  int AddNInts::result_from_resp(const std::string& resp)
  {
    mValue value;
    read(resp, value);
    mObject& resp_fields = value.get_obj();
    int result = resp_fields["result"].get_value<int>();
    return result;
  }

}
