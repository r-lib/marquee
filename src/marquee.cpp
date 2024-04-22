#include "md4c.h"

#include <cpp11/integers.hpp>
#include <cpp11/logicals.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/data_frame.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>

#include <stack>
#include <string>
#include <map>
#include <vector>

#include "named_entities.h"
#include "utf8.h"
#include "colors.h"

using namespace cpp11::literals;

std::map<std::string, cpp11::list> create_style_map(cpp11::list style_set) {
  std::map<std::string, cpp11::list> map;
  for (R_xlen_t i = 0; i < style_set.size(); ++i) {
    map[style_set.names()[i]] = cpp11::as_cpp<cpp11::list>(style_set[i]);
  }
  return map;
}

struct MARQUEE_DATA {
  std::stack<cpp11::list> style_stack;
  std::stack<std::string> type_stack;
  std::vector<size_t> index_stack;
  std::stack<int> offset_stack;
  std::stack<bool> tight_stack;
  std::map<std::string, cpp11::list> defined_styles;
  std::vector<std::string> text;
  cpp11::writable::list style;
  cpp11::writable::integers id;
  cpp11::writable::integers block;
  cpp11::writable::strings type;
  cpp11::writable::integers indent;
  cpp11::writable::integers ol_index;
  cpp11::writable::logicals tight;
  cpp11::writable::integers until;
  R_xlen_t current_id;
  unsigned current_block;
  unsigned current_indent;
  unsigned in_img;

  MARQUEE_DATA(cpp11::list styles) :
    style_stack(),
    type_stack({""}),
    index_stack(),
    offset_stack({0}),
    tight_stack({false}),
    defined_styles(create_style_map(styles)),
    text(),
    style(),
    id(),
    block(),
    type(),
    indent(),
    ol_index(),
    tight(),
    until(),
    current_id(0),
    current_block(0),
    current_indent(0),
    in_img(0) {

  }
};

static UTF_UCS utf_converter;

inline std::string entity_to_unicode(const std::string& entity) {
  std::string res;
  if (entity[1] == '#') {
    uint32_t codepoint = entity[2] == 'x' || entity[2] == 'X' ? std::stoi(entity.substr(2), NULL, 16) : std::stoi(entity.substr(1), NULL, 10);
    std::vector<uint32_t> str = {codepoint};
    int n = 0;
    res = std::string(utf_converter.convert_to_utf(str.data(), 1, n));
  } else {
    auto loc = entity_map.find(entity);
    if (loc != entity_map.end()) {
      res = loc->second;
    }
  }
  return res;
}

bool is_font_feature(SEXP elem) {
  return Rf_inherits(elem, "font_feature");
}

bool is_relative(SEXP elem) {
  return Rf_inherits(elem, "marquee_relative");
}

bool is_em(SEXP elem) {
  return Rf_inherits(elem, "marquee_em");
}

bool is_rem(SEXP elem) {
  return Rf_inherits(elem, "marquee_rem");
}

inline cpp11::writable::list combine_styles(cpp11::list parent, cpp11::list def) {
  cpp11::writable::list new_style(parent.size());
  for (R_xlen_t i = 0; i < parent.size(); ++i) {
    if (Rf_isNull(def[i])) {
      new_style[i] = parent[i];
    } else if (is_relative(def[i])) {
      double rel = REAL(VECTOR_ELT(def[i], 0))[0];
      if (Rf_isReal(parent[i])) {
        new_style[i] = Rf_ScalarReal(rel * REAL(parent[i])[0]);
      } else if (Rf_isInteger(parent[i])) {
        new_style[i] = Rf_ScalarReal(rel * INTEGER(parent[i])[0]);
      } else if (is_em(parent[i]) || is_rem(parent[i])) {
        cpp11::writable::list val(parent[i]);
        val[0] = Rf_ScalarReal(REAL(val[0])[0] * rel);
        new_style[i] = val;
      } else {
        new_style[i] = parent[i];
      }
    } else if (is_font_feature(parent[i])) {
      if (!is_font_feature(def[i])) {
        Rf_error("Malformed style object. font features can only be merged with other font features");
      }
      cpp11::writable::strings features(VECTOR_ELT(parent[i], 0));
      cpp11::writable::integers values(VECTOR_ELT(parent[i], 1));
      R_xlen_t n_old = features.size();
      cpp11::strings new_features(VECTOR_ELT(def[i], 0));
      cpp11::integers new_values(VECTOR_ELT(def[i], 1));

      for (R_xlen_t j = 0; j < new_features.size(); ++j) {
        int exist = -1;
        for (R_xlen_t k = 0; k < n_old; ++k) {
          if (features[k] == new_features[j]) {
            exist = k;
            break;
          }
        }
        if (exist >= 0) {
          values[exist] = new_values[j];
        } else {
          features.push_back(new_features[j]);
          values.push_back(new_values[j]);
        }
      }
      values.names() = features;
      cpp11::writable::list val({features, values});
      val.attr("class") = "font_feature";
      new_style[i] = val;
    } else {
      new_style[i] = def[i];
    }
  }
  new_style.attr("names") = parent.attr("names");
  return new_style;
}

inline void push_info(MARQUEE_DATA* userdata, std::string type, bool block = false, bool tight = false, int offset = 1) {
  userdata->type_stack.push(type);
  userdata->index_stack.push_back(userdata->until.size());
  auto style = userdata->defined_styles.find(type);
  if (style == userdata->defined_styles.end()) {
    if (userdata->style_stack.empty()) {
      cpp11::stop("Opening style not found");
    }
    cpp11::writable::list last_style = userdata->style_stack.top();
    if (is_color(type)) {
      last_style[2] = cpp11::writable::strings({type});
    }
    userdata->style_stack.push(last_style);
  } else if (userdata->style_stack.empty()) {
    userdata->style_stack.push(style->second);
  } else {
    cpp11::writable::list s = combine_styles(userdata->style_stack.top(), style->second);
    userdata->style_stack.push(s);
  }

  if (block) {
    userdata->current_indent++;
    if (type == "li") {
      userdata->offset_stack.top()++;
    } else {
      userdata->offset_stack.push(offset-1);
      userdata->tight_stack.push(tight);
    }
  }
}

inline void init_text(MARQUEE_DATA* userdata) {
  userdata->style.push_back(userdata->style_stack.top());
  userdata->text.push_back("");
  userdata->id.push_back(userdata->current_id);
  userdata->block.push_back(userdata->current_block);
  userdata->type.push_back(userdata->type_stack.top());
  userdata->indent.push_back(userdata->current_indent);
  userdata->ol_index.push_back(userdata->offset_stack.top());
  userdata->tight.push_back(userdata->tight_stack.top());
  userdata->until.push_back(userdata->until.size() + 1);
}

inline void pop_info(MARQUEE_DATA* userdata, std::string type, bool block = false) {
  if (!userdata->style_stack.empty()) {
    userdata->style_stack.pop();
  }
  userdata->type_stack.pop();
  size_t cur_line = userdata->until.size();
  for (size_t i = 0; i < userdata->index_stack.size(); ++i) {
    userdata->until[userdata->index_stack[i]] = cur_line;
  }
  userdata->index_stack.pop_back();
  if (block) {
    userdata->current_indent--;
    if (type != "li") {
      userdata->offset_stack.pop();
      userdata->tight_stack.pop();
    }
  }
}

inline void append_text(MARQUEE_DATA* userdata, const std::string& text) {
  userdata->text.back() += text;
}

static int enter_block_callback(MD_BLOCKTYPE type, void* detail, void* userdata) {
  MARQUEE_DATA* ud = (MARQUEE_DATA*) userdata;

  if (ud->in_img) return 0;

  switch(type) {
  case MD_BLOCK_DOC:      push_info(ud, "body", true); break;
  case MD_BLOCK_UL:       push_info(ud, "ul", true, ((MD_BLOCK_UL_DETAIL *) detail)->is_tight != 0); break;
  case MD_BLOCK_OL:       push_info(ud, "ol", true, ((MD_BLOCK_OL_DETAIL *) detail)->is_tight != 0, ((MD_BLOCK_OL_DETAIL *) detail)->start); break;
  case MD_BLOCK_LI:       push_info(ud, "li", true); break;
  case MD_BLOCK_HR:       push_info(ud, "hr", true); break;
  case MD_BLOCK_H:        push_info(ud, std::string("h") + std::to_string(((MD_BLOCK_H_DETAIL*) detail)->level), true); break;
  case MD_BLOCK_CODE:     push_info(ud, "cb", true); break;
  case MD_BLOCK_P:        push_info(ud, "p", true); break;
  case MD_BLOCK_QUOTE:    push_info(ud, "qb", true); break;
  case MD_BLOCK_HTML:
  case MD_BLOCK_TABLE:
  case MD_BLOCK_THEAD:
  case MD_BLOCK_TBODY:
  case MD_BLOCK_TR:
  case MD_BLOCK_TH:
  case MD_BLOCK_TD:       return 0;
  }

  ud->current_block++;
  init_text(ud);

  return 0;
}

static int leave_block_callback(MD_BLOCKTYPE type, void* detail, void* userdata) {
  MARQUEE_DATA* ud = (MARQUEE_DATA*) userdata;

  if (ud->in_img) return 0;

  switch(type) {
  case MD_BLOCK_DOC:      pop_info(ud, "body", true); break;
  case MD_BLOCK_UL:       pop_info(ud, "ul", true); break;
  case MD_BLOCK_OL:       pop_info(ud, "ol", true); break;
  case MD_BLOCK_LI:       pop_info(ud, "li", true); break;
  case MD_BLOCK_HR:       pop_info(ud, "hr", true); break;
  case MD_BLOCK_H:        pop_info(ud, std::string("h") + std::to_string(((MD_BLOCK_H_DETAIL*) detail)->level), true); break;
  case MD_BLOCK_CODE:     pop_info(ud, "cb", true); break;
  case MD_BLOCK_P:        pop_info(ud, "p", true); break;
  case MD_BLOCK_QUOTE:    pop_info(ud, "qb", true); break;
  case MD_BLOCK_HTML:
  case MD_BLOCK_TABLE:
  case MD_BLOCK_THEAD:
  case MD_BLOCK_TBODY:
  case MD_BLOCK_TR:
  case MD_BLOCK_TH:
  case MD_BLOCK_TD:       return 0;
  }

  return 0;
}

static int enter_span_callback(MD_SPANTYPE type, void* detail, void* userdata) {
  MARQUEE_DATA* ud = (MARQUEE_DATA*) userdata;

  if (type == MD_SPAN_IMG) {
    if (!ud->in_img) {
      push_info(ud, "img");
      init_text(ud);
      MD_SPAN_IMG_DETAIL * img_detail = (MD_SPAN_IMG_DETAIL *)  detail;
      append_text(ud, std::string(img_detail->src.text, img_detail->src.size));
    }
    ud->in_img++;
  }

  if (ud->in_img) return 0;

  switch(type) {
  case MD_SPAN_EM:                push_info(ud, "em"); break;
  case MD_SPAN_STRONG:            push_info(ud, "str"); break;
  case MD_SPAN_A:                 push_info(ud, "a"); break;
  case MD_SPAN_CODE:              push_info(ud, "code"); break;
  case MD_SPAN_U:                 push_info(ud, "u"); break;
  case MD_SPAN_DEL:               push_info(ud, "del"); break;
  case MD_SPAN_CUSTOM:            push_info(ud, std::string(((MD_SPAN_CUSTOM_DETAIL *) detail)->cls, ((MD_SPAN_CUSTOM_DETAIL *) detail)->size)); break;
  case MD_SPAN_IMG:               // Will never end here
  case MD_SPAN_LATEXMATH:
  case MD_SPAN_LATEXMATH_DISPLAY:
  case MD_SPAN_WIKILINK:          return 0;
  }

  init_text(ud);

  return 0;
}

static int leave_span_callback(MD_SPANTYPE type, void* detail, void* userdata) {
  MARQUEE_DATA* ud = (MARQUEE_DATA*) userdata;

  if (type == MD_SPAN_IMG) {
    ud->in_img--;
  }

  if (ud->in_img) return 0;

  switch(type) {
  case MD_SPAN_EM:                pop_info(ud, "em"); break;
  case MD_SPAN_STRONG:            pop_info(ud, "str"); break;
  case MD_SPAN_A:                 pop_info(ud, "a"); break;
  case MD_SPAN_CODE:              pop_info(ud, "code"); break;
  case MD_SPAN_U:                 pop_info(ud, "u"); break;
  case MD_SPAN_DEL:               pop_info(ud, "del"); break;
  case MD_SPAN_CUSTOM:            pop_info(ud, std::string(((MD_SPAN_CUSTOM_DETAIL *) detail)->cls, ((MD_SPAN_CUSTOM_DETAIL *) detail)->size)); break;
  case MD_SPAN_IMG:               pop_info(ud, "img"); break;
  case MD_SPAN_LATEXMATH:
  case MD_SPAN_LATEXMATH_DISPLAY:
  case MD_SPAN_WIKILINK:          return 0;
  }

  init_text(ud);

  return 0;
}

static int text_callback(MD_TEXTTYPE type, const MD_CHAR* text, MD_SIZE size, void* userdata) {
  MARQUEE_DATA* ud = (MARQUEE_DATA*) userdata;

  if (ud->in_img) return 0;

  switch(type) {
  case MD_TEXT_NULLCHAR:  append_text(ud, "\uFFFD"); break;
  case MD_TEXT_BR:        append_text(ud, "\n"); break;
  case MD_TEXT_SOFTBR:    append_text(ud, " "); break;
  case MD_TEXT_ENTITY:    append_text(ud, entity_to_unicode(std::string(text, size))); break;
  default:                append_text(ud, std::string(text, size)); break;
  }

  return 0;
}

[[cpp11::register]]
cpp11::writable::list marquee_c(cpp11::strings text, cpp11::list_of<cpp11::list> styles) {
  MARQUEE_DATA userdata(styles[0]);

  MD_PARSER marquee_parser = {
    0,
    MD_FLAG_NOHTML | MD_FLAG_STRIKETHROUGH | MD_FLAG_UNDERLINE,
    enter_block_callback,
    leave_block_callback,
    enter_span_callback,
    leave_span_callback,
    text_callback,
    NULL,
    NULL
  };

  for (R_xlen_t i = 0; i < text.size(); ++i) {
    userdata.current_id = i + 1;
    userdata.style_stack = std::stack<cpp11::list>();
    if (i != 0) userdata.defined_styles = create_style_map(styles[i]);

    std::string str(text[i]);

    md_parse(str.c_str(), str.size(), &marquee_parser, (void *) &userdata);
  }

  cpp11::writable::list res{
    cpp11::writable::strings(userdata.text.begin(), userdata.text.end()),
    userdata.id,
    userdata.block,
    userdata.type,
    userdata.indent,
    userdata.ol_index,
    userdata.tight,
    userdata.until
  };
  cpp11::writable::strings res_names = {"text", "id", "block", "type", "indentation", "ol_index", "tight", "ends"};

  cpp11::list doc_style(userdata.style[0]);
  std::vector<double> rem_size = {REAL(doc_style[0])[0]};
  for (R_xlen_t i = 1; i < userdata.style.size(); ++i) {
    if (userdata.id[i] != userdata.id[i - 1]) {
      rem_size.push_back(REAL(VECTOR_ELT(userdata.style[i], 0))[0]);
    }
  }
  for (R_xlen_t i = 0; i < doc_style.size(); ++i) {
    res_names.push_back(doc_style.names()[i]);

    if (i == 1) { // Background element - can have pattern so must be list
      cpp11::writable::list col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        col.push_back(VECTOR_ELT(userdata.style[j], i));
      }
      res.push_back(col);
    } else if (is_em(doc_style[i]) || is_rem(doc_style[i]) || (Rf_isReal(doc_style[i]) && Rf_xlength(doc_style[i]) == 1)) {
      cpp11::writable::doubles col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        double val = 0;
        SEXP elem = VECTOR_ELT(userdata.style[j], i);
        if (is_em(elem)) {
          val = REAL(VECTOR_ELT(userdata.style[j], 0))[0] * REAL(VECTOR_ELT(elem, 0))[0];
        } else if (is_rem(elem)) {
          int id = userdata.id[j] - 1;
          val = rem_size[id] * REAL(VECTOR_ELT(elem, 0))[0];
        } else {
          val = REAL(elem)[0];
        }
        col.push_back(val);
      }
      res.push_back(col);
    } else if (Rf_xlength(doc_style[i]) > 1 || TYPEOF(doc_style[i]) == VECSXP) {
      cpp11::writable::list col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        col.push_back(VECTOR_ELT(userdata.style[j], i));
      }
      res.push_back(col);
    } else if (Rf_isLogical(doc_style[i])) {
      cpp11::writable::logicals col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        col.push_back(LOGICAL(VECTOR_ELT(userdata.style[j], i))[0]);
      }
      res.push_back(col);
    } else if (Rf_isInteger(doc_style[i])) {
      cpp11::writable::integers col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        col.push_back(INTEGER(VECTOR_ELT(userdata.style[j], i))[0]);
      }
      res.push_back(col);
    } else if (Rf_isString(doc_style[i])) {
      cpp11::writable::strings col;
      for (R_xlen_t j = 0; j < userdata.style.size(); ++j) {
        col.push_back(STRING_ELT(VECTOR_ELT(userdata.style[j], i), 0));
      }
      res.push_back(col);
    } else {
      cpp11::warning("Ignoring unknown style type: %s", Rf_translateCharUTF8(doc_style.names()[i]));
      res_names.pop_back();
    }
  }
  res.names() = res_names;
  res.attr("class") = {"tbl_df", "tbl", "data.frame"};
  res.attr("row.names") = {R_NaInt, int(-userdata.id.size())};
  return res;
};

[[cpp11::register]]
cpp11::writable::list place_bullets(cpp11::strings type, cpp11::integers indent,
                                    cpp11::logicals string_is_empty,
                                    cpp11::integers bullet_number,
                                    cpp11::list_of<cpp11::strings> bullets) {
  static const cpp11::r_string ul("ul");
  static const cpp11::r_string ol("ol");
  cpp11::writable::integers stretch_start, stretch_end, index, placement;
  cpp11::writable::strings stretch_type, bullet;
  for (R_xlen_t i = 0; i < type.size() - 1; ++i) {
    bool is_ol = type[i] == ol;
    bool is_ul = type[i] == ul;
    if (!is_ol && !is_ul) {
      continue;
    }

    int n_ind = 0;
    for (R_xlen_t k = stretch_start.size() - 1; k >= 0; --k) {
      if (i > stretch_end[k] || stretch_type[k] == "ol") break;
      n_ind++;
    }
    cpp11::r_string ul_bullet = bullets[i][n_ind % bullets[i].size()];

    stretch_start.push_back(i);
    int end = i;
    stretch_type.push_back(type[i]);

    int ind = indent[i];
    for (R_xlen_t j = i + 1; j < type.size(); ++j) {
      end = j;
      if (indent[j] == ind) break;
      if (indent[j] == ind + 1) {
        index.push_back(j + 1);

        if (is_ol) {
          bullet.push_back(std::to_string(bullet_number[j]) + ".");
        } else {
          bullet.push_back(ul_bullet);
        }
        if (string_is_empty[j]) {
          j++;
          while (string_is_empty[j] && indent[j] > ind + 1) {
            j++;
          }
        }
        placement.push_back(j + 1);
      }
    }
    stretch_end.push_back(end);
  }

  return cpp11::writable::list({
    "bullet"_nm = bullet,
    "index"_nm = index,
    "placement"_nm = placement
  });
}

[[cpp11::register]]
cpp11::writable::logicals block_is_last(cpp11::integers indentation) {
  cpp11::writable::logicals res;

  for (R_xlen_t i = 0; i < indentation.size() - 1; ++i) {
    int ind = indentation[i];
    R_xlen_t next_block = i + 1;
    while (next_block <= indentation.size() && ind < indentation[next_block]) next_block++;
    res.push_back(next_block == indentation.size() || ind != indentation[next_block]);
  }
  res.push_back(true);

  return res;
}
