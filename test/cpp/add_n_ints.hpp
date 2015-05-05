#pragma once

#include <json/json.h>
#include <algorithm>
#include <sstream>
#include <iterator>

namespace quickmsg {

  class AddNInts
  {
  public:
    std::string create_req(const std::vector<int>& add_ints);
    std::string create_resp(const std::string& req);
    std::vector<int> vals_from_req(const std::string& req);
    int result_from_resp(const std::string& resp);
  };

  inline std::string AddNInts::create_req(const std::vector<int>& add_ints)
  {
    Json::Value obj;    
    Json::Value& ints = obj["ints_to_add"];
    std::for_each(add_ints.begin(), add_ints.end(), [&](int i){ ints.append(i); });
    std::ostringstream ss; ss << obj;
    return ss.str();
  }

  inline std::vector<int> AddNInts::vals_from_req(const std::string& req)
  {
    std::istringstream ss(req);
    Json::Value obj;
    ss >> obj;    
    Json::Value& ints = obj["ints_to_add"];
    std::vector<int> ints_to_add;
    std::for_each(ints.begin(), ints.end(), [&](Json::Value& v){ints_to_add.push_back(v.asInt());});
    return ints_to_add;
  }

  inline std::string AddNInts::create_resp(const std::string& req)
  {
    std::vector<int> ints_to_add = vals_from_req(req);
    int result = std::accumulate(ints_to_add.begin(), ints_to_add.end(), 0);    
    
    Json::Value obj;
    Json::Value& ints = obj["ints_to_add"];
    std::for_each(ints_to_add.begin(), ints_to_add.end(), [&](int i){ints.append(i);});
    obj["result"] = result;
    std::ostringstream ss; ss << obj;
    return ss.str();
  }

  inline int AddNInts::result_from_resp(const std::string& resp)
  {
    std::istringstream ss(resp);
    Json::Value obj; ss >> obj;
    return obj["result"].asInt();
  }

}
